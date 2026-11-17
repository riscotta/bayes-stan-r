"""T07.4: testes adversariais sintéticos de caminhos, inputs e não sobrescrita."""
import contextlib
import csv
import hashlib
import importlib.util
import io
import json
import os
from pathlib import Path
import subprocess
import tempfile
import unittest
from unittest.mock import patch

MODULE = Path(__file__).resolve().parents[2] / 'adapters/t07_3/runner.py'
spec = importlib.util.spec_from_file_location('t07_4_runner', MODULE)
runner = importlib.util.module_from_spec(spec)
spec.loader.exec_module(runner)


class OutputPolicy(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        self.script = self.root / 'scripts/pnadc_estudo_i/auditar_e10.py'
        self.script.parent.mkdir(parents=True)
        self.script.write_text("print('somente leitura')\n", encoding='utf-8')
        self.inp = self.root / runner.STUDIES['pnadc_estudo_i']['input']
        self.inp.parent.mkdir(parents=True)
        self.inp.write_text('evidencia_publica_sintetica\n', encoding='utf-8')
        self.manifest = self.root / 'reports/t07_2/manifesto_codigos_originais_sha256.csv'
        self.manifest.parent.mkdir(parents=True)
        with self.manifest.open('w', encoding='utf-8', newline='') as file:
            writer = csv.DictWriter(file, fieldnames=['path','sha256','bytes','git_blob_sha1'])
            writer.writeheader()
            b = self.script.read_bytes()
            writer.writerow({'path': 'scripts/pnadc_estudo_i/auditar_e10.py',
                             'sha256': hashlib.sha256(b).hexdigest(),
                             'bytes': len(b), 'git_blob_sha1': '0'*40})

    def call(self, action='executar', run_id='unit1', study='pnadc_estudo_i'):
        err = io.StringIO()
        with contextlib.redirect_stderr(err), contextlib.redirect_stdout(io.StringIO()):
            code = runner.main(['--estudo',study,'--modo','auditoria_rapida_N2',
                                '--acao',action,'--id',run_id],root=self.root)
        return code, err.getvalue()

    def test_output_root_and_type_allowlist(self):
        for study in runner.STUDIES:
            for kind in runner.OUTPUT_KINDS:
                d = runner.canonical_output(self.root, study, kind, 'n1')
                self.assertEqual(d.relative_to(self.root).parts[:3], ('outputs',study,kind))
        self.assertFalse((self.root/'outputs').exists())
        for kind in ('../data', 'other', 'logs/../data',''):
            with self.assertRaises(runner.GateError):
                runner.canonical_output(self.root, 'pnadc_estudo_i', kind, 'n1')

    def test_nonstandard_study_or_id_rejected_without_writes(self):
        for study in ('../../data','fake',''):
            with self.assertRaises(runner.GateError):
                runner.canonical_output(self.root,study,'logs','run1')
        for runid in ('../a','a/b', 'x\\y', '', 'A', 'é'):
            with self.assertRaises(runner.GateError):
                runner.canonical_output(self.root,'pnadc_estudo_i','logs',runid)
        self.assertFalse((self.root/'outputs').exists())

    def test_reservation_preserves_existing_run(self):
        d=runner.reserve_output(self.root,'pnadc_estudo_i','logs','r1')
        (d/'old.txt').write_text('historico',encoding='utf-8')
        with self.assertRaises(FileExistsError):
            runner.reserve_output(self.root,'pnadc_estudo_i','logs','r1')
        self.assertEqual((d/'old.txt').read_text(),'historico')

    def test_existing_file_or_symlink_in_parent_blocked(self):
        (self.root/'outputs').write_text('não é pasta',encoding='utf-8')
        with self.assertRaises(runner.GateError):
            runner.reserve_output(self.root,'pnadc_estudo_i','logs','r1')
        self.assertEqual((self.root/'outputs').read_text(),'não é pasta')
        (self.root/'outputs').unlink()
        (self.root/'outputs').symlink_to(self.root/'data',target_is_directory=True)
        with self.assertRaises(runner.GateError):
            runner.reserve_output(self.root,'pnadc_estudo_i','tables','r1')

    def test_atomic_exclusive_file_preserves_content(self):
        d=runner.reserve_output(self.root,'pnadc_estudo_i','logs','r1')
        runner.write_new_artifact(d,'report.json','primeiro')
        with self.assertRaises(FileExistsError):
            runner.write_new_artifact(d,'report.json','segundo')
        self.assertEqual((d/'report.json').read_text(),'primeiro')
        with self.assertRaises(runner.GateError):
            runner.write_new_artifact(d,'../data/raw.txt','x')

    def test_symlink_artifact_does_not_overwrite_target(self):
        d=runner.reserve_output(self.root,'pnadc_estudo_i','logs','r1')
        sentinel=self.root/'data'/'sentinel.txt'
        sentinel.write_text('preservado')
        (d/'stdout.txt').symlink_to(sentinel)
        with self.assertRaises(OSError):
            runner.write_new_artifact(d,'stdout.txt','não gravar')
        self.assertEqual(sentinel.read_text(),'preservado')

    def test_pnad_run_is_canonical_and_input_preserved(self):
        source_before=self.script.read_bytes(); input_before=self.inp.read_bytes()
        rc,_=self.call()
        self.assertEqual(rc,0)
        target=self.root/'outputs/pnadc_estudo_i/logs/unit1'
        self.assertEqual({p.name for p in target.iterdir()}, {'stdout.txt','stderr.txt','report.json'})
        self.assertEqual((self.script.read_bytes(),self.inp.read_bytes()),(source_before,input_before))
        self.assertEqual(self.call()[0],2)

    def test_adversarial_stdout_precreation_fails_preserving_report(self):
        orig_run=runner.subprocess.run
        target=self.root/'outputs/pnadc_estudo_i/logs/unit1'
        def precreate(*a,**kw):
            (target/'stdout.txt').write_text('não substituir',encoding='utf-8')
            return subprocess.CompletedProcess(args=a[0],returncode=0,stdout='substituto',stderr='')
        with patch.object(runner.subprocess,'run',side_effect=precreate):
            self.assertEqual(self.call()[0],2)
        self.assertEqual((target/'stdout.txt').read_text(),'não substituir')
        rec=json.loads((target/'report.json').read_text())
        self.assertEqual(rec['state'],'ERROR')
        self.assertEqual(rec['error'],'FileExistsError')

    def test_detects_modified_input_but_does_not_claim_prevention(self):
        def modifies_input(*a,**kw):
            self.inp.write_text('alterado!',encoding='utf-8')
            return subprocess.CompletedProcess(args=a[0],returncode=0,stdout='ok',stderr='')
        with patch.object(runner.subprocess,'run',side_effect=modifies_input):
            self.assertEqual(self.call()[0],2)
        rec=json.loads((self.root/'outputs/pnadc_estudo_i/logs/unit1/report.json').read_text())
        self.assertEqual(rec['state'],'FAILED_INTEGRITY')
        self.assertEqual(rec['error'],'INPUT_OR_SOURCE_CHANGED')

    def test_detects_modified_source(self):
        def modifies_source(*a,**kw):
            self.script.write_text('modificado',encoding='utf-8')
            return subprocess.CompletedProcess(args=a[0],returncode=0,stdout='ok',stderr='')
        with patch.object(runner.subprocess,'run',side_effect=modifies_source):
            self.assertEqual(self.call()[0],2)
        rec=json.loads((self.root/'outputs/pnadc_estudo_i/logs/unit1/report.json').read_text())
        self.assertEqual(rec['state'],'FAILED_INTEGRITY')

    def test_manifest_ancestor_symlink_rejected_before_output(self):
        external=self.root/'external';external.mkdir()
        backup=external/'manifesto_codigos_originais_sha256.csv'
        backup.write_bytes(self.manifest.read_bytes())
        self.manifest.unlink();self.manifest.parent.rmdir()
        (self.root/'reports/t07_2').symlink_to(external,target_is_directory=True)
        self.assertEqual(self.call()[0],2)
        self.assertFalse((self.root/'outputs').exists())

    def test_blocked_historical_routes_never_reserve_outputs(self):
        for study in ('mortalidade_sp_0_74','caes_raca_f5'):
            self.assertEqual(self.call(study=study)[0],2)
        self.assertFalse((self.root/'outputs').exists())

    def test_directory_symlink_swap_after_plan_is_rechecked(self):
        runner.plan('pnadc_estudo_i','unit1',self.root)
        (self.root/'outputs').symlink_to(self.root/'data',target_is_directory=True)
        self.assertEqual(self.call()[0],2)
        self.assertEqual(list((self.root/'data').glob('pnadc_estudo_i/logs/*')),[])


if __name__=='__main__':
    unittest.main()

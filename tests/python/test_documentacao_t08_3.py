#!/usr/bin/env python3
"""Protótipo GE T08.3: fixtures sintéticas e regressão da raiz, sem rede nem execução científica.
Não é parser GFM completo nem suíte de aceitação integral; cobertura A1/A2 pendente.
"""
import re
import shlex
import unittest
from collections import Counter
from dataclasses import dataclass
from pathlib import Path, PurePosixPath
from urllib.parse import unquote, urlsplit


@dataclass(frozen=True)
class Result:
    status: str
    target: str = ""


def heading_ids(text):
    """Subconjunto ATX de GFM. IDs duplicados e explícitos preservados."""
    ids, used = set(), Counter()
    for _, line in visible_lines(text):
        for match in re.finditer(r'<a\s+(?:id|name)=[\'"]([^\'"]+)[\'"]\s*>', line, re.I):
            ids.add(match[1])
        m = re.match(r'^ {0,3}#{1,6}\s+(.+?)\s*#*\s*$', line)
        if m:
            slug = re.sub(r'[^\w\- ]', '', m[1].lower()).replace(' ', '-')
            i = used[slug]
            candidate = slug if not i else f'{slug}-{i}'
            while candidate in ids:
                i += 1
                candidate = f'{slug}-{i}'
            ids.add(candidate)
            used[slug] = i + 1
    return ids


def visible_lines(text):
    fence = None
    for i, line in enumerate(text.splitlines(), 1):
        m = re.match(r'^ {0,3}(`{3,}|~{3,})(.*)$', line)
        if m:
            tok, rest = m.groups()
            if fence is None:
                fence = tok
                continue
            if tok[0] == fence[0] and len(tok) >= len(fence) and not rest.strip():
                fence = None
                continue
        if fence is None and not line.startswith(('    ', '\t')):
            yield i, line


def resolve_link(origin, raw, tree, markdown, protected=(), symlinks=(), *,
                 repository=None, base_sha=None, source_line_counts=None,
                 confirmed_github_paths=()):
    # Não aceitar controles que urlsplit possa remover, nem encoding recursivo.
    if raw != raw.strip() or any(ord(c) < 32 or ord(c) == 127 for c in raw):
        return Result('CAMINHO_NAO_SUPORTADO')
    if re.search(r'%25[0-9a-fA-F]{2}', raw, re.I):
        return Result('ENCODING_RECURSIVO_NAO_VERIFICADO')
    # urlsplit separa query/fragmento antes de decodificar; nunca faz download.
    if re.search(r'%(?![0-9a-fA-F]{2})', raw):
        return Result('ENCODING_INVALIDO')
    try:
        url = urlsplit(raw)
        path = unquote(url.path, errors='strict')
        fragment = unquote(url.fragment, errors='strict')
    except (UnicodeError, ValueError):
        return Result('ENCODING_INVALIDO')
    if url.scheme or url.netloc:
        if url.scheme != 'https' or url.netloc != 'github.com' or not repository:
            return Result('EXTERNO_NAO_VERIFICADO' if repository else 'EXTERNO_OU_GITHUB_INTERNO_NAO_VERIFICADO')
        prefix = '/' + repository + '/'
        if not path.startswith(prefix):
            return Result('EXTERNO_NAO_VERIFICADO')
        route = path[len(prefix):]
        if url.query:
            return Result('GITHUB_QUERY_NAO_VERIFICADA')
        if route.startswith('blob/'):
            sha, sep, rest = route[5:].partition('/')
            if not sep or not base_sha or sha != base_sha:
                return Result('GITHUB_REF_NAO_FIXADA')
            # Caminho absoluto no repositório, não relativo à origem do Markdown.
            origin = 'README.md'
            path = rest
        elif route.startswith('tree/'):
            return Result('GITHUB_TREE_TIPO_NAO_VERIFICADO')
        else:
            # Metadados confirmados não comprovam âncoras em páginas dinâmicas.
            if fragment:
                return Result('GITHUB_FRAGMENTO_NAO_VERIFICADO')
            if route in confirmed_github_paths:
                return Result('GITHUB_EXISTENCIA_METADADOS')
            return Result('GITHUB_METADADO_NAO_VERIFICADO')
    if url.query:
        return Result('QUERY_NAO_VERIFICADA')
    if '\\' in path or any(ord(c) < 32 or ord(c) == 127 for c in path) or path.startswith('/') or re.match(r'^[A-Za-z]:', path):
        return Result('CAMINHO_NAO_SUPORTADO')
    parts = list(PurePosixPath(origin).parent.parts) if path else []
    for part in (path if path else origin).split('/'):
        if part in ('', '.'):
            continue
        if part == '..':
            if not parts:
                return Result('ESCAPE_RAIZ')
            parts.pop()
        else:
            parts.append(part)
    target = '/'.join(parts)
    # Não expõe nem consulta conteúdo de destino protegido.
    for root in protected:
        # Erro de caixa não pode fazer o auditor publicar um caminho protegido.
        if target.casefold() == root.casefold() or target.casefold().startswith(root.rstrip('/').casefold() + '/'):
            return Result('PROTEGIDO_EXISTENCIA' if target in tree else 'PROTEGIDO_AUSENTE_OU_CASE')
    for root in symlinks:
        if target == root or target.startswith(root.rstrip('/') + '/'):
            return Result('SYMLINK_NAO_VERIFICADO')
    if target not in tree:
        if any(p.casefold() == target.casefold() for p in tree):
            return Result('CASE_INCORRETO', target)
        return Result('DESTINO_AUSENTE', target)
    if fragment:
        if target not in markdown:
            n = (source_line_counts or {}).get(target)
            match = re.fullmatch(r'L([1-9][0-9]*)(?:-L([1-9][0-9]*))?', fragment)
            if n is None or not match:
                return Result('FRAGMENTO_NAO_VERIFICADO', target)
            first = int(match[1]); last = int(match[2] or first)
            return Result('LINHAS_EXISTENCIA' if 1 <= first <= last <= n else 'LINHAS_FORA_INTERVALO', target)
        if fragment not in heading_ids(markdown[target]):
            return Result('FRAGMENTO_AUSENTE_NO_SUBCONJUNTO', target)
    return Result('OK_SUBCONJUNTO', target)


# Limita-se a destinos sem espaços não escapados, a referências de uma linha
# e atributos HTML entre aspas. Casos mais complexos devem ser inconclusivos.
INLINE = re.compile(r'!?\[[^\]\n]*\]\(<?([^\s)>]+)>?(?:\s+"[^"]*")?\)')
REFDEF = re.compile(r'^ {0,3}\[([^\]]+)\]:\s*<?([^\s>]+)>?')
REFUSE = re.compile(r'!?\[([^\]]+)\]\[([^\]]*)\]')
# Caso explícito de imagem clicável (badge). Duas ocorrências, não uma.
NESTED_IMAGE = re.compile(r'\[!\[[^\]\n]*\]\(([^\s)]+)\)\]\(([^\s)]+)\)')
HTML = re.compile(r'<(?:a|img)\b[^>]*?\b(?:href|src)=[\'"]([^\'"]+)[\'"]', re.I)


def extract_links(text):
    lines = list(visible_lines(text))
    refs = {}
    for _, line in lines:
        m = REFDEF.match(line)
        if m:
            refs[' '.join(m[1].lower().split())] = m[2]
    for n, line in lines:
        if REFDEF.match(line):
            continue
        line = re.sub(r'`[^`]*`', '', line)
        for m in NESTED_IMAGE.finditer(line):
            yield n, m[1]
            yield n, m[2]
        line = NESTED_IMAGE.sub('', line)
        for pat in (INLINE, HTML):
            for m in pat.finditer(line):
                yield n, m[1]
        for m in REFUSE.finditer(line):
            key = ' '.join((m[2] or m[1]).lower().split())
            yield n, refs.get(key)


def classify_command(command, tree, contracts, suspended=(), *, required_flags=()):
    """Contrato fornecido explicitamente pela fixture; não infere flags por regex."""
    if re.search(r'[;&|`\n]|\$\(', command):
        return 'SHELL_NAO_VERIFICADO'
    if re.search(r'<[^>]+>|\{[A-Za-z_]+\}', command):
        return 'ILUSTRATIVO'
    try:
        args = shlex.split(command, posix=True)
    except ValueError:
        return 'COMANDO_NAO_VERIFICADO'
    if not args or args[0] not in {'Rscript', 'python', 'python3'}:
        return 'COMANDO_NAO_VERIFICADO'
    args = args[1:]
    if args and args[0] == '--vanilla':
        args = args[1:]
    if not args:
        return 'COMANDO_NAO_VERIFICADO'
    entry = args[0]
    if entry not in tree:
        return 'ENTRYPOINT_AUSENTE'
    if entry in suspended:
        return 'SUSPENSO'
    if entry not in contracts:
        return 'CONTRATO_NAO_VERIFICADO'
    seen = set()
    for arg in args[1:]:
        if arg.startswith('--'):
            flag, sep, value = arg.partition('=')
            if flag not in contracts[entry]:
                return 'FLAG_INVALIDA'
            if flag in seen:
                return 'FLAG_DUPLICADA_NAO_VERIFICADA'
            seen.add(flag)
            allowed = contracts[entry][flag]
            if allowed is None and sep:
                return 'VALOR_NAO_VERIFICADO_OU_INVALIDO'
            if allowed is not None and (not sep or value not in allowed):
                return 'VALOR_NAO_VERIFICADO_OU_INVALIDO'
        else:
            return 'ARGUMENTO_NAO_VERIFICADO'
    if not set(required_flags).issubset(seen):
        return 'FLAG_OBRIGATORIA_AUSENTE'
    return 'OK_CONTRATO_SINTETICO'


class TestT083Preflight(unittest.TestCase):
    def setUp(self):
        self.docs = {'README.md': '# Início\n# Seção\n# Seção\n<a id="manual"></a>',
                     'docs/guia.md': '# Guia\n## Ação'}
        self.tree = set(self.docs) | {'docs', 'scripts/check.py', 'data/restrito.csv',
                                     'docs/imagem.png', 'docs/arquivo com espaço.md', 'alias'}
        self.contracts = {'scripts/check.py': {'--modo': {'auditar'}, '--quiet': None}}
        self.assertEqual(self.link('docs/guia.md#ação').status, 'OK_SUBCONJUNTO')
        self.assertEqual(self.command('python scripts/check.py --modo=auditar'), 'OK_CONTRATO_SINTETICO')

    def link(self, raw):
        return resolve_link('README.md', raw, self.tree, self.docs,
                            protected=('data/restrito.csv',), symlinks=('alias',))

    def command(self, raw, suspended=()):
        return classify_command(raw, self.tree, self.contracts, suspended)

    def test_broken_link(self):
        self.assertEqual(self.link('docs/guia.md').status, 'OK_SUBCONJUNTO')
        self.assertEqual(self.link('docs/falta.md').status, 'DESTINO_AUSENTE')

    def test_case(self):
        self.assertEqual(self.link('docs/guia.md').status, 'OK_SUBCONJUNTO')
        self.assertEqual(self.link('docs/Guia.md').status, 'CASE_INCORRETO')

    def test_fragment(self):
        self.assertEqual(self.link('docs/guia.md#ação').status, 'OK_SUBCONJUNTO')
        self.assertEqual(self.link('docs/guia.md#inexistente').status, 'FRAGMENTO_AUSENTE_NO_SUBCONJUNTO')

    def test_duplicate_unicode_and_explicit(self):
        for anchor in ('seção', 'seção-1', 'manual', 'se%C3%A7%C3%A3o'):
            with self.subTest(anchor=anchor):
                self.assertEqual(self.link('#' + anchor).status, 'OK_SUBCONJUNTO')
        self.assertEqual(self.link('#seção-2').status, 'FRAGMENTO_AUSENTE_NO_SUBCONJUNTO')

    def test_escape(self):
        self.assertEqual(self.link('README.md').status, 'OK_SUBCONJUNTO')
        self.assertEqual(self.link('%2e%2e/README.md').status, 'ESCAPE_RAIZ')

    def test_encoding(self):
        self.assertEqual(self.link('docs/arquivo%20com%20espa%C3%A7o.md').status, 'OK_SUBCONJUNTO')
        self.assertEqual(self.link('docs/%zz').status, 'ENCODING_INVALIDO')

    def test_protected(self):
        self.assertEqual(self.link('data/restrito.csv#qualquer'), Result('PROTEGIDO_EXISTENCIA'))

    def test_symlink(self):
        self.assertEqual(self.link('alias').status, 'SYMLINK_NAO_VERIFICADO')

    def test_relative(self):
        self.assertEqual(resolve_link('docs/guia.md', '../README.md#manual', self.tree, self.docs).status, 'OK_SUBCONJUNTO')

    def test_reference_html_images_and_fences(self):
        md = '[guia][g]\n[g]: docs/guia.md#ação\n![img](docs/imagem.png)\n<a href="README.md#manual">x</a>\n```md\n[falso](missing.md)\n```\n`[literal](missing.md)`\n'
        self.assertEqual(list(extract_links(md)), [(1, 'docs/guia.md#ação'), (3, 'docs/imagem.png'), (4, 'README.md#manual')])
        self.assertEqual(list(extract_links('[guia][falta]')), [(1, None)])

    def test_placeholder(self):
        self.assertEqual(self.command('Rscript scripts/<pasta>/<script>.R'), 'ILUSTRATIVO')

    def test_flag(self):
        self.assertEqual(self.command('python scripts/check.py --modo=auditar'), 'OK_CONTRATO_SINTETICO')
        self.assertEqual(self.command('python scripts/check.py --modoo=auditar'), 'FLAG_INVALIDA')

    def test_flag_value(self):
        self.assertEqual(self.command('python scripts/check.py --modo=auditar'), 'OK_CONTRATO_SINTETICO')
        self.assertEqual(self.command('python scripts/check.py --modo=reestimar'), 'VALOR_NAO_VERIFICADO_OU_INVALIDO')

    def test_suspended(self):
        self.assertEqual(self.command('python scripts/check.py --modo=auditar'), 'OK_CONTRATO_SINTETICO')
        self.assertEqual(self.command('python scripts/check.py --modo=auditar', ('scripts/check.py',)), 'SUSPENSO')

    def test_entrypoint(self):
        self.assertEqual(self.command('python scripts/check.py'), 'OK_CONTRATO_SINTETICO')
        self.assertEqual(self.command('python scripts/falta.py'), 'ENTRYPOINT_AUSENTE')

    def test_shell(self):
        self.assertEqual(self.command('python scripts/check.py'), 'OK_CONTRATO_SINTETICO')
        self.assertEqual(self.command('python scripts/check.py; rm -rf /'), 'SHELL_NAO_VERIFICADO')

    def test_external(self):
        self.assertEqual(self.link('https://example.org/doc').status, 'EXTERNO_OU_GITHUB_INTERNO_NAO_VERIFICADO')


    def test_nested_badge_two_occurrences(self):
        self.assertEqual(list(extract_links('[![CI](https://example.org/badge.svg)](README.md)')),
                         [(1, 'https://example.org/badge.svg'), (1, 'README.md')])

    def test_line_range_bounds(self):
        opts = {'source_line_counts': {'scripts/check.py': 12}}
        for frag, status in [('L1-L12','LINHAS_EXISTENCIA'), ('L12','LINHAS_EXISTENCIA'),
                             ('L12-L13','LINHAS_FORA_INTERVALO'), ('L9-L2','LINHAS_FORA_INTERVALO'),
                             ('L0','FRAGMENTO_NAO_VERIFICADO')]:
            with self.subTest(fragment=frag):
                self.assertEqual(resolve_link('README.md', 'scripts/check.py#'+frag,
                    self.tree, self.docs, **opts).status, status)

    def test_protected_wrong_case_no_disclosure(self):
        self.assertEqual(self.link('DATA/RESTRITO.CSV#L1'), Result('PROTEGIDO_AUSENTE_OU_CASE'))

    def test_fixed_github_blob_and_mutable_ref(self):
        repo, sha = 'owner/repo', 'a'*40
        def call(ref):
            return resolve_link('docs/guia.md', 'https://github.com/'+repo+'/blob/'+ref+'/README.md#seção',
                self.tree, self.docs, repository=repo, base_sha=sha)
        self.assertEqual(call(sha), Result('OK_SUBCONJUNTO','README.md'))
        self.assertEqual(call('main'), Result('GITHUB_REF_NAO_FIXADA'))
        self.assertEqual(call('b'*40), Result('GITHUB_REF_NAO_FIXADA'))

    def test_github_metadata_no_implicit_fragment(self):
        def call(suffix):
            return resolve_link('README.md','https://github.com/owner/repo/'+suffix,
                self.tree, self.docs, repository='owner/repo', confirmed_github_paths={'issues/14'})
        self.assertEqual(call('issues/14').status, 'GITHUB_EXISTENCIA_METADADOS')
        self.assertEqual(call('issues/14#issuecomment-1').status, 'GITHUB_FRAGMENTO_NAO_VERIFICADO')
        self.assertEqual(call('issues/15').status, 'GITHUB_METADADO_NAO_VERIFICADO')
        self.assertEqual(call('issues/14?x=1').status, 'GITHUB_QUERY_NAO_VERIFICADA')

    def test_github_protected_blob_no_disclosure(self):
        sha = 'a'*40
        self.assertEqual(resolve_link('README.md', 'https://github.com/owner/repo/blob/'+sha+'/data/restrito.csv#L1',
            self.tree, self.docs, protected=('data/restrito.csv',), repository='owner/repo', base_sha=sha),
            Result('PROTEGIDO_EXISTENCIA'))

    def test_query_and_control_character(self):
        self.assertEqual(self.link('README.md').status, 'OK_SUBCONJUNTO')
        self.assertEqual(self.link('README.md?raw=1').status,'QUERY_NAO_VERIFICADA')
        self.assertEqual(self.link('docs/%0Aguia.md').status,'CAMINHO_NAO_SUPORTADO')



    def test_control_and_whitespace_baseline_mutation(self):
        self.assertEqual(self.link('docs/guia.md').status, 'OK_SUBCONJUNTO')
        for mutant in (' docs/guia.md', 'docs/guia.md ', 'docs/%7Fguia.md'):
            with self.subTest(mutant=mutant):
                self.assertEqual(self.link(mutant).status, 'CAMINHO_NAO_SUPORTADO')

    def test_required_flag_baseline_mutation(self):
        def classify(raw):
            return classify_command(raw, self.tree, self.contracts, required_flags=('--modo',))
        self.assertEqual(classify('python scripts/check.py --modo=auditar'), 'OK_CONTRATO_SINTETICO')
        self.assertEqual(classify('python scripts/check.py'), 'FLAG_OBRIGATORIA_AUSENTE')

    def test_duplicate_flag_baseline_mutation(self):
        self.assertEqual(self.command('python scripts/check.py --modo=auditar'), 'OK_CONTRATO_SINTETICO')
        self.assertEqual(self.command('python scripts/check.py --modo=auditar --modo=auditar'), 'FLAG_DUPLICADA_NAO_VERIFICADA')

    def test_boolean_flag_baseline_mutation(self):
        self.assertEqual(self.command('python scripts/check.py --quiet'), 'OK_CONTRATO_SINTETICO')
        self.assertEqual(self.command('python scripts/check.py --quiet=no'), 'VALOR_NAO_VERIFICADO_OU_INVALIDO')

    def test_placeholder_with_shell_baseline_mutation(self):
        self.assertEqual(self.command('python scripts/<pasta>/<script>.py'), 'ILUSTRATIVO')
        self.assertEqual(self.command('python scripts/<pasta>/<script>.py; echo x'), 'SHELL_NAO_VERIFICADO')



def readme_diagnostics(text):
    """Regressão específica da raiz: não é validador GFM geral."""
    errors = []
    if '§' in text:
        errors.append('DELIMITADOR_CORROMPIDO')
    fences = []
    active = None
    for line in text.splitlines():
        m = re.match(r'^ {0,3}(`{3,}|~{3,})(.*)$', line)
        if m:
            tok, rest = m.groups()
            if active is None:
                active = (tok, rest.strip(), [])
            elif tok[0] == active[0][0] and len(tok) >= len(active[0]) and not rest.strip():
                fences.append((active[1], tuple(active[2])))
                active = None
            else:
                active[2].append(line)
        elif active is not None:
            active[2].append(line)
    if active is not None:
        errors.append('CERCA_ABERTA')
    return errors, fences


class TestT083ReadmeRegression(unittest.TestCase):
    FIXTURE = "# Título\n\nUse `token`.\n\n```bash\n# comentário\npython script.py\n```\n"

    def setUp(self):
        self.assertEqual(readme_diagnostics(self.FIXTURE), ([], [('bash', ('# comentário', 'python script.py'))]))

    def test_corrupted_inline(self):
        self.assertIn('DELIMITADOR_CORROMPIDO', readme_diagnostics(self.FIXTURE.replace('`token`', '§token§'))[0])

    def test_corrupted_fences(self):
        errors, blocks = readme_diagnostics(self.FIXTURE.replace('```', '§§§'))
        self.assertIn('DELIMITADOR_CORROMPIDO', errors)
        self.assertEqual(blocks, [])

    def test_unclosed_fence(self):
        self.assertIn('CERCA_ABERTA', readme_diagnostics(self.FIXTURE.rsplit('```', 1)[0])[0])

    def test_comment_not_heading(self):
        self.assertEqual(heading_ids(self.FIXTURE), {'título'})
        self.assertIn('comentário', heading_ids(self.FIXTURE.replace('```', '§§§')))

    def test_safe_combination(self):
        fixture = '<details>\n<summary>Exemplo</summary>\n\n$z=1$\n\n</details>\n\n[![badge](https://example.org/b.svg)](README.md)\n' + self.FIXTURE
        self.assertEqual(readme_diagnostics(fixture)[0], [])
        self.assertEqual(list(extract_links(fixture)), [(8, 'https://example.org/b.svg'), (8, 'README.md')])

    def test_candidate_root_readme(self):
        # Em CI, path é a raiz do checkout. Em FZ, é um recorte local explícito.
        root = Path(__file__).resolve().parents[2]
        text = (root / 'README.md').read_text(encoding='utf-8')
        errors, blocks = readme_diagnostics(text)
        self.assertEqual(errors, [])
        shell = [lines for lang, lines in blocks if lang == 'bash']
        self.assertEqual(len(shell), 3)
        for cmd in ('Rscript scripts/therapeutic_touch/therapeutic_touch.R',
                    'Rscript scripts/_setup/install_cmdstan.R', 'Rscript tests/run_tests.R'):
            self.assertEqual(sum(cmd in lines for lines in shell), 1)
        for span in ('`scripts/`', '`setwd()`', '`cmdstanr`', '`rstan`', '`renv.lock`',
                     '`renv`', '`Rscript scripts/_setup/install_deps.R`', '`--run_stan=1`',
                     '`outputs/<estudo>/<tipo>/`', '`R-hat`', '`testthat`'):
            self.assertIn(span, text)

    def test_recursive_encoding_fail_closed(self):
        tree = {'README.md', 'data/restrito.csv'}
        baseline = resolve_link('README.md', 'data/restrito.csv', tree, {}, protected=('data',))
        self.assertEqual(baseline, Result('PROTEGIDO_EXISTENCIA'))
        self.assertEqual(resolve_link('README.md', 'data%252Frestrito.csv', tree, {}, protected=('data',)), Result('ENCODING_RECURSIVO_NAO_VERIFICADO'))
        self.assertEqual(resolve_link('README.md', '\ndata/restrito.csv', tree, {}, protected=('data',)), Result('CAMINHO_NAO_SUPORTADO'))


if __name__ == '__main__':
    unittest.main(verbosity=2)

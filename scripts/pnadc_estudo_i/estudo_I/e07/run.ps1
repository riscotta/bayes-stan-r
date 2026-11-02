param(
    [ValidateSet('preflight','smoke','completo','resumo')]
    [string]$Modo = 'preflight',
    [string]$PastaResultados = ''
)
$ErrorActionPreference = 'Stop'
$Raiz = Split-Path -Parent $MyInvocation.MyCommand.Path
$Rscript = $null
if ($env:RSCRIPT_EXE -and (Test-Path -LiteralPath $env:RSCRIPT_EXE -PathType Leaf)) {
    $Rscript = $env:RSCRIPT_EXE
}
if (-not $Rscript) {
    $encontrado = Get-Command 'Rscript.exe' -ErrorAction SilentlyContinue
    if ($encontrado) { $Rscript = $encontrado.Source }
}
if (-not $Rscript) {
    foreach ($chave in @('HKLM:\SOFTWARE\R-core\R', 'HKCU:\SOFTWARE\R-core\R')) {
        $inst = Get-ItemProperty -Path $chave -ErrorAction SilentlyContinue
        if ($inst -and $inst.InstallPath) {
            foreach ($sufixo in @('bin\Rscript.exe', 'bin\x64\Rscript.exe')) {
                $candidato = Join-Path $inst.InstallPath $sufixo
                if (Test-Path -LiteralPath $candidato -PathType Leaf) { $Rscript = $candidato; break }
            }
        }
        if ($Rscript) { break }
    }
}
if (-not $Rscript) {
    foreach ($raizR in @($env:ProgramFiles, \${env:ProgramFiles(x86)})) {
        if (-not $raizR) { continue }
        $dirR = Join-Path $raizR 'R'
        if (-not (Test-Path -LiteralPath $dirR -PathType Container)) { continue }
        $pastas = Get-ChildItem -LiteralPath $dirR -Directory -ErrorAction SilentlyContinue | Sort-Object Name -Descending
        foreach ($pasta in $pastas) {
            foreach ($sufixo in @('bin\Rscript.exe', 'bin\x64\Rscript.exe')) {
                $candidato = Join-Path $pasta.FullName $sufixo
                if (Test-Path -LiteralPath $candidato -PathType Leaf) { $Rscript = $candidato; break }
            }
            if ($Rscript) { break }
        }
        if ($Rscript) { break }
    }
}
if (-not $Rscript) { throw 'Rscript.exe nao encontrado. O preflight nao instala R; informe RSCRIPT_EXE se necessario.' }
Write-Host "Rscript localizado: $Rscript"
switch ($Modo) {
    'preflight' { $Script = Join-Path $Raiz 'R\preflight_e07.R'; $Argumentos = @() }
    'smoke'     { $Script = Join-Path $Raiz 'R\ajustar_marginais_e07.R'; $Argumentos = @('smoke') }
    'completo'  { $Script = Join-Path $Raiz 'R\ajustar_marginais_e07.R'; $Argumentos = @('completo') }
    'resumo' {
        if (-not $PastaResultados) { throw 'Para resumo, informe -PastaResultados com a pasta da execucao anterior.' }
        $Script = Join-Path $Raiz 'R\reprocessar_resumos_e07.R'
        $Argumentos = @($PastaResultados)
    }
}
& $Rscript '--vanilla' $Script @Argumentos
if ($LASTEXITCODE -ne 0) { throw "Falha E07 ($Modo); Rscript retornou codigo $LASTEXITCODE. Arquivos preexistentes preservados." }
Write-Host "Etapa operacional '$Modo' concluida."

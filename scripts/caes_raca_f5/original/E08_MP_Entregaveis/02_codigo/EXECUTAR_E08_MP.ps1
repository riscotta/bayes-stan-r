param(
  [ValidateSet('verify','prior','smoke','full')]
  [string]$Etapa = 'verify'
)
Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'
$Root = Split-Path -Parent $PSScriptRoot
$Runner = Join-Path $PSScriptRoot 'executar_E08_MP.R'
$R = Get-Command 'Rscript' -ErrorAction Stop
$Stamp = Get-Date -Format 'yyyyMMdd_HHmmss_ffff'
$OutputDir = Join-Path $Root "07_retornos\resultado_extensao_E08_MP_${Etapa}_$Stamp"
New-Item -ItemType Directory -Path $OutputDir -ErrorAction Stop | Out-Null
$Stdout = Join-Path $OutputDir 'Rscript_stdout.txt'
$Stderr = Join-Path $OutputDir 'Rscript_stderr.txt'
# Logs da chamada Rscript devem ficar FORA de OutputDir ate o R liberar seu guard de pasta vazia.
$StdoutTemp = "$OutputDir.stdout.tmp.txt"
$StderrTemp = "$OutputDir.stderr.tmp.txt"
$Argumentos = @('--vanilla', ('"' + $Runner + '"'), "--stage=$Etapa", ('--output="' + $OutputDir + '"'))
Push-Location $Root
try {
  try {
    $Proc = Start-Process -FilePath $R.Source -ArgumentList $Argumentos -NoNewWindow -Wait -PassThru `
      -RedirectStandardOutput $StdoutTemp -RedirectStandardError $StderrTemp
  } finally {
    # Sempre preservar os logs no diretorio de retorno, inclusive quando Rscript falhar.
    if (Test-Path -LiteralPath $StdoutTemp) { Move-Item -LiteralPath $StdoutTemp -Destination $Stdout -Force }
    if (Test-Path -LiteralPath $StderrTemp) { Move-Item -LiteralPath $StderrTemp -Destination $Stderr -Force }
  }
  if ($Proc.ExitCode -ne 0) {
    Write-Host "Rscript falhou (exit=$($Proc.ExitCode)). Log: $Stderr"
    if(Test-Path $Stderr) { Get-Content $Stderr -Tail 40 | Write-Host }
    throw "Rscript retornou exit code $($Proc.ExitCode); pacote de sucesso NAO gerado."
  }
  $MarkerPath = Join-Path $OutputDir 'MARCADOR_EXECUCAO_COMPUTACIONAL.json'
  @{ stage=$Etapa; exit_code=0; instante=(Get-Date).ToUniversalTime().ToString('o'); `
      status='EXECUTADO_NAO_VALIDADO'; nota='Nao aprova MP1/MP4 nem indica validacao externa' } |
    ConvertTo-Json -Depth 5 | Set-Content -Path $MarkerPath -Encoding UTF8
  $Files = Get-ChildItem $OutputDir -Recurse -File | Where-Object { $_.Name -ne 'MANIFESTO_RETORNO_SHA256.csv' }
  $Rows = foreach ($F in $Files) {
    [PSCustomObject]@{ arquivo=$F.FullName.Substring($OutputDir.Length + 1).Replace('\','/'); `
      sha256=(Get-FileHash -LiteralPath $F.FullName -Algorithm SHA256).Hash.ToLowerInvariant(); bytes=$F.Length }
  }
  $Rows | Export-Csv -Path (Join-Path $OutputDir 'MANIFESTO_RETORNO_SHA256.csv') -Encoding UTF8 -NoTypeInformation
  $FinalZip = "$OutputDir.zip"
  $TmpZip = "$OutputDir.tmp.zip"
  $Archived = $false
  for($Attempt=1; $Attempt -le 4; $Attempt++) {
    try {
      if(Test-Path $TmpZip) { Remove-Item $TmpZip -Force }
      Compress-Archive -Path (Join-Path $OutputDir '*') -DestinationPath $TmpZip -CompressionLevel Optimal -ErrorAction Stop
      if(-not (Test-Path $TmpZip)) { throw 'ZIP nao encontrado' }
      if((Get-Item $TmpZip).Length -lt 1024) { throw 'ZIP anormalmente pequeno' }
      Move-Item -Path $TmpZip -Destination $FinalZip -ErrorAction Stop
      $Archived = $true
      break
    } catch {
      if($Attempt -ge 4) { throw }
      Start-Sleep -Seconds 2
    }
  }
  if(-not $Archived) { throw 'Compactacao nao completada' }
  $ZipHash = (Get-FileHash -Path $FinalZip -Algorithm SHA256).Hash.ToLowerInvariant()
  Write-Host "PACOTE DE RETORNO (NAO VALIDADO): $FinalZip"
  Write-Host "SHA256: $ZipHash"
} finally {
  Pop-Location
}

$ErrorActionPreference = "Stop"

function populate_defaults {
    $script:DEFAULT_OWNER= Read-Host 'Default GitHub username'
    $script:DEFAULT_NAME=Read-Host 'Default name'
    $script:DEFAULT_EMAIL=Read-Host 'Default email address'
}
function set_defaults{
    Move-Item hs-init.hs hs-init.orig.hs
    foreach($line in Get-Content -Encoding UTF8 hs-init.orig.hs){
        $line = $line -replace "^(defaultOwner) = ""(.*)""$","`$1 = ""$DEFAULT_OWNER"""
        $line = $line -replace "^(defaultName) = ""(.*)""$","`$1 = ""$DEFAULT_NAME"""
        $line = $line -replace "^(defaultEmail) = ""(.*)""$","`$1 = ""$DEFAULT_EMAIL"""
        $line | Out-File -encoding UTF8 -Append hs-init.hs
    }
    Remove-Item hs-init.orig.hs
}

function install_dependencies{
    $packages = Select-String -Pattern "--package (.*)" -Path hs-init.hs | % {($_.matches.groups[1].value)}
    stack install $packages
    if($LASTEXITCODE -ne 0){
        exit 1
    }
}

$TempDir = "_hs-init_install_" + [GUID]::NewGuid()
$TargetDir = "$env:LOCALAPPDATA\hs-init"

Write-Host "Using temporary directory $TempDir"

$null = New-Item $TempDir -ItemType Directory
Set-Location $TempDir

$hsFile = "https://raw.githubusercontent.com/vrom911/hs-init/master/hs-init.hs"

Write-Host "Downloading $hsFile"

Invoke-Webrequest $hsFile -OutFile .\hs-init.hs

populate_defaults

Write-Host "modifying hs-init.hs with selected defaults"
set_defaults

Write-Host "Installing dependencies"
install_dependencies

Write-Host "compiling"

stack ghc -- -O2 hs-init.hs
if($LASTEXITCODE -ne 0){
    exit 1
}

Write-Host "Installing hs-init.exe to $TargetDir"
$null = New-Item $TargetDir -ItemType Directory -Force
Move-Item .\hs-init.exe "$TargetDir\hs-init.exe" -Force

Write-Host "Adding $TargetDir to Path"
[System.Environment]::SetEnvironmentVariable("Path", $env:Path + ";$env:LOCALAPPDATA\hs-init",[System.EnvironmentVariableTarget]::User)

Write-Host "Cleaning up Temp Dir $TempDir"
Set-Location ..
Remove-Item $TempDir -Recurse

Write-Host "Installation Complete"

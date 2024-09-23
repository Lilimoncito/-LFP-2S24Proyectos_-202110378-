# -*- mode: python ; coding: utf-8 -*-


a = Analysis(
    ['interfaz.py'],
    pathex=[],
    binaries=[],
    datas=[('C:\\Users\\cesar\\OneDrive\\Documentos\\Fortran\\Proyecto1_202110378\\Fortran\\Main.exe', '.'), ('C:\\Users\\cesar\\OneDrive\\Documentos\\Fortran\\Proyecto1_202110378\\banderas', 'banderas'), ('C:\\Users\\cesar\\OneDrive\\Documentos\\Fortran\\Proyecto1_202110378\\datos_saturacion.txt', '.'), ('C:\\Users\\cesar\\OneDrive\\Documentos\\Fortran\\Proyecto1_202110378\\entrada.txt', '.'), ('C:\\Users\\cesar\\OneDrive\\Documentos\\Fortran\\Proyecto1_202110378\\errores_tokens.html', '.'), ('C:\\Users\\cesar\\OneDrive\\Documentos\\Fortran\\Proyecto1_202110378\\grafica.dot', '.'), ('C:\\Users\\cesar\\OneDrive\\Documentos\\Fortran\\Proyecto1_202110378\\grafica.png', '.'), ('C:\\Users\\cesar\\OneDrive\\Documentos\\Fortran\\Proyecto1_202110378\\tokens.html', '.')],
    hiddenimports=[],
    hookspath=[],
    hooksconfig={},
    runtime_hooks=[],
    excludes=[],
    noarchive=False,
    optimize=0,
)
pyz = PYZ(a.pure)

exe = EXE(
    pyz,
    a.scripts,
    a.binaries,
    a.datas,
    [],
    name='interfaz',
    debug=False,
    bootloader_ignore_signals=False,
    strip=False,
    upx=True,
    upx_exclude=[],
    runtime_tmpdir=None,
    console=False,
    disable_windowed_traceback=False,
    argv_emulation=False,
    target_arch=None,
    codesign_identity=None,
    entitlements_file=None,
)

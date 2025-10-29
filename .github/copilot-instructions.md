## Copilot / AI assistant instructions for SolidWorks_Automation

Be concise. This file highlights project-specific structure, workflows, and examples so an AI can be productive quickly.

- Root purpose: a large, production-ready SolidWorks automation suite composed of C# add-ins, macros (C#, Python, VBA), templates and many documentation guides. Key areas: `macros/csharp`, `templates/`, `codestack/`, `docs/`, and `solidworks-api/`.

- Quick tasks (examples):
  - To build the main add-in: open `macros\csharp\Solidworks-Automation\Solidworks Automation.sln` in Visual Studio 2022 (run as Administrator) and press Ctrl+Shift+B.
  - Run one-time setup scripts from repo root in an elevated PowerShell prompt: `.
    SETUP_TEMPLATE_INTEGRATION.ps1` and `.
    SETUP_HEADER_SECTION_TOOL.ps1`.
  - Python utilities: install dependencies with `pip install -r requirements.txt` and use scripts under `utilities/python`.

- Patterns and conventions you must follow or reference:
  - Excel-driven configuration: many tools (Header Section Tool, XCH Structure Tool) use Excel files as canonical config. Look for files named like `*_HCS.xlsx`, `XCH_SCS.xlsx`, and other `000000_*.xlsx` files in the templates and tools folders.
  - Job number conventions: job identifiers generally use `S2####` prefixes; code and documentation expect these formats in searches and UI filters.
  - Template prefixes: `HUD_`, `ZST_`, `AXC_VAULT` references indicate certified/production template naming and locations; avoid modifying large binary template files unless requested and reviewed.
  - Add-in architecture: the primary automation logic lives in the C# add-in (COM) under `macros/csharp/Solidworks-Automation`. Many UI flows call into this add-in; prefer editing the add-in for cross-cutting changes.

- Integration points and external dependencies to watch for:
  - SolidWorks (host application) — add-in interacts via SolidWorks API (SolidDNA framework present under `solidworks-api/SolidDna`). Do not attempt to run or test add-in code without a SolidWorks instance available.
  - Excel files as inputs — scripts read/write Excel; keep Excel schema stable unless updating all callers and docs.
  - PowerShell setup scripts & environment configuration (`SETUP_*` and `Auto-Organize.ps1`) which configure large local folders and template copies; these are destructive if run incorrectly — flag changes to these scripts for human review.
  - Python interop: `requirements.txt` lists `pywin32`, `comtypes`, `pythonnet` — these are used for Windows COM interop and require Windows/CPython environment.

- Where to find definitive docs and examples:
  - High-level project README: `README.md` (repo root) — architecture overview and quick-start.
  - Learning examples and API samples: `codestack/README.md` and `solidworks-api/` (many sample READMEs under tutorials).
  - Integration guides: `docs/` contains `HEADER_SECTION_TOOL_INTEGRATION.md`, `JOB_BROWSER_INTEGRATION.md`, `XCH_STRUCTURE_TOOL_INTEGRATION.md` — reference these before changing schema or behavior.

- Safety and change guidance for automated edits:
  - Avoid modifying binary templates or the large template collections (e.g. `templates/`, `output/`) in automated PRs; instead, create patches that change code or small text files and propose manual steps for template refreshes.
  - When altering Excel-driven config formats, update the corresponding `docs/*_INTEGRATION.md` and search for all callers (C# add-in + any Python scripts) to update their parsing logic.

- Good first PR examples for a Copilot agent:
  - Fix a documentation typo or broken link in `docs/` and add a short verification note in the doc.
  - Add a small helper function in `utilities/python` and a short README example demonstrating pip usage (`requirements.txt`).
  - Update a single C# utility class with adequate unit-like smoke checks (but do not change large project wiring without human review).

If something here is unclear or you'd like me to expand any section (build, run, or integration examples), tell me which area and I'll iterate.

Verified paths (checked 2025-10-28)
---------------------------------
Use these exact paths when scripting or opening files in the workspace:

- Setup scripts:
  - `scripts/setup/SETUP_HEADER_SECTION_TOOL.ps1`
  - `scripts/setup/SETUP_TEMPLATE_INTEGRATION.ps1`

- Add-in solution:
  - `macros/csharp/Solidworks-Automation/Solidworks Automation.sln`

- Example Header Section config (HCS) files:
  - `templates/header_section_tool/Combined_/Drafting/Headers/000000_S01c-HCS.xlsx`
  - `templates/header_section_tool/Single_/Drafting/Headers/000000_S03-HCS.xlsx`

Quick PowerShell: list all HCS files
----------------------------------
If you need a quick list of all Header Section config files, run this from the repo root in PowerShell:

```powershell
Get-ChildItem -Path templates/header_section_tool -Recurse -Include "*_HCS.xlsx" | Select-Object FullName
```

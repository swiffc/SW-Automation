# AI Agents Guide (AGENTS.md)

Purpose
-------
This file expands the short Copilot instructions into a pragmatic, step-by-step guide for automated agents and contributors who will make small, safe changes to the repository. It focuses on concrete workflows (build, setup, search, edits, PRs) and shows examples tailored to this codebase.

Essential context
-----------------
- Root project: enterprise SolidWorks automation suite with a C# add-in, many templates, Excel-driven configuration, and helper scripts.
- Key locations:
  - Add-in solution: `macros/csharp/Solidworks-Automation/Solidworks Automation.sln`
  - Templates & outputs: `templates/`, `output/` (large binary files—avoid automated edits)
  - Excel-driven tools: `templates/header_section_tool/`, `templates/xch_structure_tool/`, `templates/z_structure_tool/`
  - Documentation and guides: `docs/` (many integration guides like `HEADER_SECTION_TOOL_INTEGRATION.md`)
  - Learning examples: `codestack/` and `solidworks-api/`

Platform & dependencies
-----------------------
- Development assumptions:
  - OS: Windows (SolidWorks only runs on Windows)
  - Visual Studio 2022 for the C# add-in
  - Python (CPython on Windows) for utilities under `utilities/python`
  - Excel installed when testing Excel-driven automation
- Python dependencies: install via `pip install -r requirements.txt` (includes `pywin32`, `comtypes`, `pythonnet`).

Quick start (most common agent tasks)
-----------------------------------
1) Run one-time setup (Admin PowerShell required):

```powershell
# From repo root (run PowerShell as Administrator)
.\SETUP_TEMPLATE_INTEGRATION.ps1
.\SETUP_HEADER_SECTION_TOOL.ps1
```

2) Build the add-in (open in Visual Studio 2022 as Administrator):

```powershell
# Open the solution path and build inside Visual Studio (preferred)
# macros\csharp\Solidworks-Automation\Solidworks Automation.sln
# Or use msbuild from Developer Command Prompt:
# msbuild "macros\csharp\Solidworks-Automation\Solidworks Automation.sln" /p:Configuration=Release
```

3) Install Python dependencies for scripts:

```powershell
pip install -r requirements.txt
```

Search patterns & quick finds
-----------------------------
Use these examples to find config and entry points:

- Find Excel config files (PowerShell):

```powershell
Get-ChildItem -Path . -Recurse -Include "*_HCS.xlsx","XCH_SCS.xlsx","000000_*" | Select-Object FullName
```

- Find C# add-in entry (look for the main solution and namespace):

```powershell
Get-ChildItem -Recurse -Filter "*.sln" | Select-String -Pattern "Solidworks-Automation"
```

- Search for a specific doc or integration guide (PowerShell):

```powershell
Select-String -Path docs\*.md -Pattern "HEADER_SECTION_TOOL" -List
```

Safe-edit guidelines (what agents must not change automatically)
---------------------------------------------------------------
- Do NOT modify binary template files in `templates/` or `output/` in automated PRs. These are large, production templates and require manual review.
- Avoid changing PowerShell setup scripts (`SETUP_*`) without explicit human review; they modify local folders and copy templates.
- When changing Excel-driven formats or file names (for example `000000_S01c-HCS.xlsx`), also update the corresponding integration docs in `docs/` and search/patch all callers in `macros/csharp` and `utilities/python`.

Concrete edit workflows (examples)
---------------------------------
Below are a few repeatable, safe workflows with examples.

1) Small Python helper in `utilities/python`

- Goal: add a helper that reads an Excel mapping and prints a summary.
- Files to edit: create `utilities/python/excel_summary.py`, update `requirements.txt` if needed, add short README under `utilities/python/README.md`.
- Steps:
  1. Create the new file with a clear function and a small `if __name__ == '__main__'` smoke run.
  2. Run `pip install -r requirements.txt` then `python utilities\python\excel_summary.py` locally to verify it runs (it should not require SolidWorks).
  3. Commit on branch `feature/python-excel-summary` and open a PR that includes the README and a short test instruction.

Example commit message:

```
feat: add excel_summary helper (utilities/python)

Adds a small CLI utility to summarize Excel mapping files. No production impact.
```

2) Edit a C# utility class (small refactor)

- Goal: change a helper in `macros/csharp/Solidworks-Automation/ModelTools/`.
- Steps:
  1. Open solution in Visual Studio 2022. Build to ensure a clean baseline.
  2. Make minimal, focused change in the utility class. Keep public API stable.
  3. Rebuild the solution (Ctrl+Shift+B) and confirm no compile errors.
  4. Run any smoke/manual scenario in SolidWorks if available (human-required for full validation). If SolidWorks is not available, verify compilation and unit-like checks (if present).
  5. Update `docs/` if the behavior or config changed.
  6. Branch naming: `fix/<short>` or `feat/<short>`. Include a PR description linking to relevant docs.

3) Change Excel schema used by a tool (higher-risk)

- Goal: rename a column or change expected worksheet layout for Header Section Tool.
- Required steps:
  1. Search for all callers referencing the Excel filename or columns. Example search terms: the filename `000000_S01c-HCS.xlsx`, worksheet names, column headers used in code.
  2. Update code in `macros/csharp` and any Python scripts under `utilities/python` that parse the Excel file.
  3. Update `docs/HEADER_SECTION_TOOL_INTEGRATION.md` with the new schema and an example Excel file in `templates/header_section_tool/` if appropriate.
  4. Add regression verification steps to the PR describing how to validate with a sample Excel (human verification required with SolidWorks open).
  5. Request at least one human reviewer with domain knowledge (Excel-driven automation).

PR checklist (what to include in a PR)
------------------------------------
- Clear title and description explaining the change and the reason.
- List of files changed and why (if non-obvious).
- For code changes: show build status from your machine (e.g., "Built solution locally in Release mode, no compile errors").
- For Excel/schema changes: include a link to updated docs and a short verification script or steps to validate.
- For non-trivial changes: include a short risk assessment and rollback plan.
- NEVER include large binary template file updates in an automated PR. If templates must change, create a separate manual PR and clearly note human steps for verification.

Branch/commit naming and PR handling
-----------------------------------
- Branches: `feat/<short>`, `fix/<short>`, `chore/<short>`
- Commit messages: one-line summary followed by a short body explaining the why and how.
- PR description: include reference to the integration docs and any manual steps needed for testing.

Quality gates & validation
--------------------------
- Build: always ensure the C# solution builds in Visual Studio 2022. If you change C#, run a local build before committing.
- Lint/typecheck: there is no repo-wide automated linter configured; follow the existing C# and Python style where practical.
- Tests: this repository is primarily integration-style (SolidWorks). Unit tests are rare—if you add tests, keep them fast and self-contained and document how to run them.

When you are blocked
--------------------
- If a change requires SolidWorks to validate (add-in behavior), note it clearly and request a human reviewer who can open SolidWorks and run the scenario.
- If a change touches certified templates or `AXC_VAULT` integration, flag it for manual review—these are production artifacts.

Where to find more info
-----------------------
- Root README: `README.md` — architecture, quick-start, and high-level module list.
- Integration guides: `docs/HEADER_SECTION_TOOL_INTEGRATION.md`, `docs/JOB_BROWSER_INTEGRATION.md`, `docs/XCH_STRUCTURE_TOOL_INTEGRATION.md`.
- Learning examples and API references: `codestack/` and `solidworks-api/`.

Feedback and iteration
----------------------
If this guide is missing a workflow you need automated, tell me which workflow (example: CI tasks, automated smoke tests, or a specific file edit pattern) and I'll add a step-by-step template for it.

Verified paths (checked 2025-10-28)
---------------------------------
The agent verified the existence of the most commonly-referenced files and example configs while authoring this guide. Use these exact paths when scripting or opening files:

- Setup scripts:
  - `scripts/setup/SETUP_HEADER_SECTION_TOOL.ps1`
  - `scripts/setup/SETUP_TEMPLATE_INTEGRATION.ps1`

- Add-in solution:
  - `macros/csharp/Solidworks-Automation/Solidworks Automation.sln`

- Example Header Section config (HCS) files found:
  - `templates/header_section_tool/Combined_/Drafting/Headers/000000_S01c-HCS.xlsx`
  - `templates/header_section_tool/Single_/Drafting/Headers/000000_S03-HCS.xlsx`

Quick PowerShell: list all HCS files
----------------------------------
If you need a quick list of all Header Section config files, run this from the repo root in PowerShell:

```powershell
Get-ChildItem -Path templates/header_section_tool -Recurse -Include "*-HCS.xlsx" | Select-Object FullName
```

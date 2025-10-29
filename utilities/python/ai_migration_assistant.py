#!/usr/bin/env python3
"""
AI-Powered Migration Assistant
Helps migrate legacy code to modern patterns

Usage:
    python ai_migration_assistant.py --file "Hood/HoodForm.cs" --to wpf-mvvm
    python ai_migration_assistant.py --component Hood --to modern
    python ai_migration_assistant.py --analyze "path/to/LegacyCode.cs"

Requirements:
    - OpenAI API key in .env
"""

import os
import sys
import argparse
from pathlib import Path
from openai import OpenAI

class MigrationAssistant:
    """AI-powered code migration assistant"""
    
    def __init__(self):
        self.api_key = os.getenv('OPENAI_API_KEY')
        if not self.api_key:
            raise ValueError("OPENAI_API_KEY not found in .env")
        self.client = OpenAI(api_key=self.api_key)
        self.project_root = Path(__file__).parent.parent.parent
        self.code_dir = self.project_root / "macros" / "csharp" / "Solidworks-Automation"
    
    def load_modern_example(self) -> str:
        """Load modern WPF/MVVM example as reference"""
        
        modern_files = [
            self.code_dir / "UnifiedUI" / "ViewModels" / "MainViewModel.cs",
            self.code_dir / "UnifiedUI" / "Services" / "SolidWorksService.cs"
        ]
        
        examples = []
        for file in modern_files:
            if file.exists():
                examples.append(f"// {file.name}\n" + file.read_text(encoding='utf-8')[:3000])
        
        return "\n\n".join(examples)
    
    def analyze_legacy_code(self, code: str) -> dict:
        """Analyze legacy code and identify migration needs"""
        
        prompt = f"""Analyze this legacy C# code and identify what needs modernization:

LEGACY CODE:
{code[:5000]}

ANALYSIS REQUIRED:
1. Current patterns used (WinForms, direct event handlers, etc.)
2. Anti-patterns present (no error handling, COM leaks, etc.)
3. What should be modernized?
4. Estimated migration effort (Low/Medium/High)
5. Benefits of migration
6. Migration risks

Respond in JSON format:
{{
    "current_patterns": ["pattern1", "pattern2"],
    "anti_patterns": ["issue1", "issue2"],
    "modernization_needs": {{
        "architecture": "what to change",
        "error_handling": "what to add",
        "com_safety": "what to fix"
    }},
    "effort": "Low|Medium|High",
    "benefits": ["benefit1", "benefit2"],
    "risks": ["risk1", "risk2"]
}}
"""
        
        response = self.client.chat.completions.create(
            model="gpt-4",
            messages=[
                {"role": "system", "content": "You are an expert at modernizing legacy C# codebases."},
                {"role": "user", "content": prompt}
            ],
            temperature=0.3,
            response_format={"type": "json_object"}
        )
        
        import json
        return json.loads(response.choices[0].message.content)
    
    def migrate_to_wpf_mvvm(self, legacy_code: str) -> str:
        """Migrate WinForms code to WPF/MVVM"""
        
        modern_example = self.load_modern_example()
        
        prompt = f"""Migrate this legacy WinForms code to modern WPF/MVVM pattern:

LEGACY CODE:
{legacy_code[:6000]}

MODERN PATTERN EXAMPLE:
{modern_example}

MIGRATION REQUIREMENTS:
1. Convert to MVVM architecture
2. Replace event handlers with ICommand
3. Add data binding (INotifyPropertyChanged)
4. Use async/await for long operations
5. Add GlobalErrorHandler error handling
6. Ensure COM safety with ComObjectManager
7. Add XML documentation
8. Follow .cursorrules standards

OUTPUT:
Three files:
1. ViewModel (.cs)
2. View (.xaml)
3. Model/Configuration (.cs) if needed

Provide complete, production-ready code.
"""
        
        response = self.client.chat.completions.create(
            model="gpt-4",
            messages=[
                {"role": "system", "content": "You are an expert at WPF/MVVM architecture and SolidWorks API integration."},
                {"role": "user", "content": prompt}
            ],
            temperature=0.3,
            max_tokens=4000
        )
        
        return response.choices[0].message.content
    
    def modernize_code(self, code: str, focus: str = "all") -> str:
        """Apply modern patterns to code"""
        
        prompt = f"""Modernize this C# code with focus on: {focus}

CODE:
{code[:6000]}

MODERNIZATION CHECKLIST:
1. Error Handling:
   - Add try-catch blocks
   - Use GlobalErrorHandler.LogError(ex, context)
   - User-friendly error messages

2. COM Safety:
   - Use ComObjectManager
   - Proper disposal of COM objects
   - No COM leaks

3. Null Safety:
   - Add null checks
   - Use null-conditional operators (?.)
   - Guard clauses at method start

4. Async/Await:
   - Convert long operations to async
   - Use Task-based async pattern
   - Add cancellation tokens

5. Modern C# Features:
   - Use expression-bodied members
   - Pattern matching
   - String interpolation
   - Collection initializers

6. Documentation:
   - XML comments on public methods
   - Clear parameter descriptions
   - Usage examples

Output the modernized code.
"""
        
        response = self.client.chat.completions.create(
            model="gpt-4",
            messages=[
                {"role": "system", "content": "You are an expert C# developer specializing in code modernization."},
                {"role": "user", "content": prompt}
            ],
            temperature=0.3,
            max_tokens=4000
        )
        
        migrated = response.choices[0].message.content
        
        # Clean up markdown
        if "```csharp" in migrated:
            migrated = migrated.split("```csharp")[1].split("```")[0].strip()
        elif "```" in migrated:
            migrated = migrated.split("```")[1].split("```")[0].strip()
        
        return migrated
    
    def save_migrated_code(self, code: str, original_file: Path, suffix: str = "_Migrated"):
        """Save migrated code"""
        
        output_dir = self.project_root / "output" / "migrated_code"
        output_dir.mkdir(parents=True, exist_ok=True)
        
        new_filename = original_file.stem + suffix + original_file.suffix
        output_file = output_dir / new_filename
        
        output_file.write_text(code, encoding='utf-8')
        return output_file


def main():
    parser = argparse.ArgumentParser(description='AI-powered migration assistant')
    parser.add_argument('--file', help='File to migrate')
    parser.add_argument('--component', help='Component to migrate (Hood, Header, etc.)')
    parser.add_argument('--to', choices=['wpf-mvvm', 'modern'], default='modern', help='Migration target')
    parser.add_argument('--analyze', help='Analyze code without migrating')
    parser.add_argument('--focus', choices=['all', 'error-handling', 'com-safety', 'async'], 
                       default='all', help='Focus area for modernization')
    
    args = parser.parse_args()
    
    try:
        assistant = MigrationAssistant()
        
        if args.analyze:
            file_path = Path(args.analyze)
            if not file_path.exists():
                print(f"? File not found: {args.analyze}")
                sys.exit(1)
            
            code = file_path.read_text(encoding='utf-8')
            
            print(f"\n{'='*60}")
            print(f"?? ANALYZING: {file_path.name}")
            print(f"{'='*60}\n")
            
            analysis = assistant.analyze_legacy_code(code)
            
            print(f"?? CURRENT PATTERNS:")
            for pattern in analysis.get('current_patterns', []):
                print(f"  • {pattern}")
            
            print(f"\n??  ANTI-PATTERNS:")
            for issue in analysis.get('anti_patterns', []):
                print(f"  • {issue}")
            
            print(f"\n?? MODERNIZATION NEEDS:")
            for area, need in analysis.get('modernization_needs', {}).items():
                print(f"  • {area}: {need}")
            
            print(f"\n?? EFFORT: {analysis.get('effort', 'Unknown')}")
            print(f"\n? BENEFITS:")
            for benefit in analysis.get('benefits', []):
                print(f"  • {benefit}")
            
            print(f"\n??  RISKS:")
            for risk in analysis.get('risks', []):
                print(f"  • {risk}")
            print()
        
        elif args.file:
            file_path = Path(args.file)
            if not file_path.exists():
                print(f"? File not found: {args.file}")
                sys.exit(1)
            
            code = file_path.read_text(encoding='utf-8')
            
            print(f"\n{'='*60}")
            print(f"?? MIGRATING: {file_path.name}")
            print(f"{'='*60}\n")
            
            if args.to == 'wpf-mvvm':
                print("?? Converting to WPF/MVVM...")
                migrated = assistant.migrate_to_wpf_mvvm(code)
            else:
                print(f"?? Modernizing (focus: {args.focus})...")
                migrated = assistant.modernize_code(code, args.focus)
            
            print(f"\n{'='*60}")
            print(f"? MIGRATED CODE:")
            print(f"{'='*60}\n")
            print(migrated)
            
            output_file = assistant.save_migrated_code(migrated, file_path)
            print(f"\n?? Saved to: {output_file.relative_to(assistant.project_root)}")
        
        else:
            print("Usage:")
            print("  python ai_migration_assistant.py --analyze 'path/to/LegacyCode.cs'")
            print("  python ai_migration_assistant.py --file 'Hood/HoodForm.cs' --to wpf-mvvm")
            print("  python ai_migration_assistant.py --file 'Component.cs' --to modern --focus error-handling")
    
    except Exception as e:
        print(f"\n? Error: {e}\n")
        sys.exit(1)


if __name__ == "__main__":
    main()


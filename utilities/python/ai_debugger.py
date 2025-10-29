#!/usr/bin/env python3
"""
AI-Powered Debugging Assistant
Analyzes error logs and suggests fixes

Usage:
    python ai_debugger.py "$env:APPDATA\UnifiedUIApp\Logs\latest.log"
    python ai_debugger.py --interactive
    python ai_debugger.py --error "NullReferenceException at line 245"

Requirements:
    - OpenAI API key in .env
"""

import os
import sys
import argparse
import subprocess
from pathlib import Path
from datetime import datetime
from openai import OpenAI

class AIDebugger:
    """AI-powered debugging assistant"""
    
    def __init__(self):
        self.api_key = os.getenv('OPENAI_API_KEY')
        if not self.api_key:
            raise ValueError("OPENAI_API_KEY not found in .env")
        self.client = OpenAI(api_key=self.api_key)
        self.project_root = Path(__file__).parent.parent.parent
    
    def get_recent_git_changes(self, max_lines: int = 500) -> str:
        """Get recent code changes from git"""
        try:
            result = subprocess.run(
                ['git', 'diff', 'HEAD~5..HEAD'],
                capture_output=True,
                text=True,
                cwd=self.project_root
            )
            diff = result.stdout[:max_lines * 50]  # Limit size
            return diff if diff else "No recent changes found"
        except:
            return "Git changes not available"
    
    def find_latest_log(self) -> Path:
        """Find the most recent log file"""
        
        log_locations = [
            Path(os.getenv('APPDATA')) / "UnifiedUIApp" / "Logs",
            Path(os.getenv('TEMP')),
            self.project_root / "logs"
        ]
        
        latest_log = None
        latest_time = None
        
        for location in log_locations:
            if not location.exists():
                continue
            
            for log_file in location.glob("*.log"):
                if latest_log is None or log_file.stat().st_mtime > latest_time:
                    latest_log = log_file
                    latest_time = log_file.stat().st_mtime
        
        return latest_log
    
    def analyze_error(self, error_log: str, recent_changes: str = None) -> dict:
        """Analyze error and get AI suggestions"""
        
        if recent_changes is None:
            recent_changes = self.get_recent_git_changes()
        
        prompt = f"""You are an expert C# and SolidWorks API debugger. Analyze this error:

ERROR LOG:
{error_log[-4000:]}  # Last 4000 chars

RECENT CODE CHANGES:
{recent_changes[:2000] if recent_changes else "No recent changes"}

PROJECT CONTEXT:
- 22 C# projects (.NET Framework 4.8)
- SolidWorks API COM interop
- Excel-driven configuration (Prego system)
- 7 component types (Bundle, Header, Hood, etc.)
- MVVM architecture in UnifiedUI
- GlobalErrorHandler for logging
- ComObjectManager for COM safety

COMMON ERROR PATTERNS:
1. NullReferenceException - Usually Excel data not initialized
2. COMException - SolidWorks not running or COM not released
3. TargetInvocationException - Constructor parameter issues
4. FileNotFoundException - Template paths incorrect
5. Build errors - Missing references or internal classes

ANALYSIS REQUIRED:
1. What caused this error? (root cause)
2. Which file and line number?
3. What method/class is failing?
4. Is this related to recent code changes?
5. What's the fix? (specific code changes)
6. How to prevent this in future?

Respond in JSON format:
{{
    "error_type": "exception name",
    "root_cause": "explanation",
    "file": "file path",
    "line": number,
    "method": "method name",
    "related_to_changes": true/false,
    "fix": {{
        "description": "what to change",
        "code_before": "current code",
        "code_after": "fixed code"
    }},
    "prevention": ["future prevention steps"],
    "related_errors": ["similar issues to check"]
}}
"""
        
        response = self.client.chat.completions.create(
            model="gpt-4",
            messages=[
                {"role": "system", "content": "You are an expert debugger for SolidWorks API automation projects. Provide actionable, specific fixes."},
                {"role": "user", "content": prompt}
            ],
            temperature=0.2,
            response_format={"type": "json_object"}
        )
        
        import json
        return json.loads(response.choices[0].message.content)
    
    def display_analysis(self, analysis: dict):
        """Display debugging analysis"""
        
        print(f"\n{'='*60}")
        print(f"?? AI DEBUGGING ANALYSIS")
        print(f"{'='*60}\n")
        
        print(f"? ERROR TYPE: {analysis.get('error_type', 'Unknown')}\n")
        
        print(f"?? ROOT CAUSE:")
        print(f"   {analysis.get('root_cause', 'Unknown')}\n")
        
        if analysis.get('file'):
            print(f"?? LOCATION:")
            print(f"   File: {analysis['file']}")
            if analysis.get('line'):
                print(f"   Line: {analysis['line']}")
            if analysis.get('method'):
                print(f"   Method: {analysis['method']}")
            print()
        
        if analysis.get('related_to_changes'):
            print(f"??  RELATED TO RECENT CHANGES: YES\n")
        
        if analysis.get('fix'):
            fix = analysis['fix']
            print(f"? FIX:")
            print(f"   {fix.get('description', '')}\n")
            
            if fix.get('code_before'):
                print(f"   BEFORE:")
                for line in fix['code_before'].split('\n')[:10]:
                    print(f"   {line}")
                print()
            
            if fix.get('code_after'):
                print(f"   AFTER:")
                for line in fix['code_after'].split('\n')[:10]:
                    print(f"   {line}")
                print()
        
        if analysis.get('prevention'):
            print(f"???  PREVENTION:")
            for step in analysis['prevention']:
                print(f"   • {step}")
            print()
        
        if analysis.get('related_errors'):
            print(f"?? CHECK THESE TOO:")
            for error in analysis['related_errors']:
                print(f"   • {error}")
            print()
    
    def interactive_mode(self):
        """Interactive debugging session"""
        
        print(f"\n{'='*60}")
        print(f"?? AI DEBUGGING ASSISTANT - INTERACTIVE MODE")
        print(f"{'='*60}\n")
        
        print("Options:")
        print("  1. Analyze latest log file")
        print("  2. Analyze specific log file")
        print("  3. Describe error manually")
        print()
        
        choice = input("Select option (1-3): ").strip()
        
        if choice == "1":
            log_file = self.find_latest_log()
            if not log_file:
                print("? No log files found")
                return
            
            print(f"\n?? Found: {log_file}")
            error_log = log_file.read_text(encoding='utf-8', errors='ignore')
            
        elif choice == "2":
            path = input("Log file path: ").strip()
            log_file = Path(path)
            if not log_file.exists():
                print(f"? File not found: {path}")
                return
            
            error_log = log_file.read_text(encoding='utf-8', errors='ignore')
            
        elif choice == "3":
            print("\nDescribe the error (multi-line, Ctrl+Z then Enter when done):")
            error_log = sys.stdin.read()
        else:
            print("? Invalid choice")
            return
        
        print(f"\n?? Analyzing error with AI...\n")
        analysis = self.analyze_error(error_log)
        self.display_analysis(analysis)


def main():
    parser = argparse.ArgumentParser(description='AI-powered debugging assistant')
    parser.add_argument('log_file', nargs='?', help='Path to log file')
    parser.add_argument('--interactive', action='store_true', help='Interactive mode')
    parser.add_argument('--error', help='Error description to analyze')
    parser.add_argument('--latest', action='store_true', help='Analyze latest log file')
    
    args = parser.parse_args()
    
    try:
        debugger = AIDebugger()
        
        if args.interactive:
            debugger.interactive_mode()
        
        elif args.latest:
            log_file = debugger.find_latest_log()
            if not log_file:
                print("? No log files found")
                sys.exit(1)
            
            print(f"\n?? Analyzing: {log_file}\n")
            error_log = log_file.read_text(encoding='utf-8', errors='ignore')
            analysis = debugger.analyze_error(error_log)
            debugger.display_analysis(analysis)
        
        elif args.error:
            print(f"\n?? Analyzing error description...\n")
            analysis = debugger.analyze_error(args.error)
            debugger.display_analysis(analysis)
        
        elif args.log_file:
            log_file = Path(args.log_file)
            if not log_file.exists():
                print(f"? File not found: {args.log_file}")
                sys.exit(1)
            
            print(f"\n?? Analyzing: {log_file}\n")
            error_log = log_file.read_text(encoding='utf-8', errors='ignore')
            analysis = debugger.analyze_error(error_log)
            debugger.display_analysis(analysis)
        
        else:
            print("Usage:")
            print("  python ai_debugger.py <log_file>")
            print("  python ai_debugger.py --latest")
            print("  python ai_debugger.py --interactive")
            print("  python ai_debugger.py --error 'NullReferenceException at line 245'")
    
    except Exception as e:
        print(f"\n? Error: {e}\n")
        sys.exit(1)


if __name__ == "__main__":
    main()


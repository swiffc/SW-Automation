"""
AI-Powered Repository Assistant
================================
Interactive assistant that helps developers navigate and work with the SolidWorks automation codebase.

Features:
- Answers questions using project documentation
- Suggests code patterns from existing examples
- Finds similar implementations
- Generates starter code following project conventions

Setup:
1. Copy .env.example to .env
2. Add your OpenAI API key to .env
3. Run: pip install -r requirements.txt

Usage:
    python ai_repo_assistant.py

    Or for single questions:
    python ai_repo_assistant.py "How do I add a new Excel-driven tool?"
"""

import os
import sys
from pathlib import Path
from typing import Dict, List
import json

try:
    from openai import OpenAI
    from dotenv import load_dotenv
except ImportError:
    print("Error: Required packages not installed.")
    print("Run: pip install -r requirements.txt")
    sys.exit(1)


class RepoAssistant:
    """AI assistant for navigating the SolidWorks automation repository."""
    
    def __init__(self, repo_root: Path = None):
        """Initialize the assistant with repository context."""
        # Load environment variables
        load_dotenv()
        
        self.api_key = os.getenv('OPENAI_API_KEY')
        if not self.api_key or self.api_key == 'your-openai-api-key-here':
            raise ValueError(
                "OpenAI API key not found!\n"
                "1. Copy .env.example to .env\n"
                "2. Add your OpenAI API key to .env\n"
                "3. Get key from: https://platform.openai.com/api-keys"
            )
        
        self.client = OpenAI(api_key=self.api_key)
        self.model = os.getenv('OPENAI_MODEL', 'gpt-4-turbo-preview')
        
        # Set repository root
        if repo_root is None:
            repo_root = Path(__file__).parent.parent.parent
        self.repo_root = Path(repo_root)
        
        # Load project context
        self.context = self._load_context()
        
        # Conversation history
        self.messages = []
        self._initialize_system_prompt()
    
    def _load_context(self) -> Dict[str, str]:
        """Load all relevant documentation and guides."""
        context = {}
        
        # Load key documentation files
        docs_to_load = [
            ('.github/copilot-instructions.md', 'copilot_instructions'),
            ('.github/AGENTS.md', 'agents_guide'),
            ('AGENTS.md', 'agents_guide_root'),
            ('README.md', 'readme'),
            ('docs/HEADER_SECTION_TOOL_INTEGRATION.md', 'header_section_tool'),
            ('docs/JOB_BROWSER_INTEGRATION.md', 'job_browser'),
            ('docs/XCH_STRUCTURE_TOOL_INTEGRATION.md', 'xch_structure_tool'),
        ]
        
        for file_path, key in docs_to_load:
            full_path = self.repo_root / file_path
            if full_path.exists():
                try:
                    # Try UTF-8 first, fall back to UTF-8 with error handling
                    try:
                        context[key] = full_path.read_text(encoding='utf-8')
                    except UnicodeDecodeError:
                        context[key] = full_path.read_text(encoding='utf-8', errors='ignore')
                except Exception as e:
                    print(f"Warning: Could not read {file_path}: {e}")
        
        # Load utilities README if exists
        utils_readme = self.repo_root / 'utilities' / 'python' / 'README.md'
        if utils_readme.exists():
            try:
                context['utilities_readme'] = utils_readme.read_text(encoding='utf-8')
            except UnicodeDecodeError:
                context['utilities_readme'] = utils_readme.read_text(encoding='utf-8', errors='ignore')
        
        return context
    
    def _initialize_system_prompt(self):
        """Set up the system prompt with repository context."""
        system_message = f"""You are an expert assistant for the SolidWorks Automation repository.

Your role:
- Help developers navigate and work with this enterprise SolidWorks automation codebase
- Suggest code patterns that follow established project conventions
- Provide accurate file paths and examples from the repository
- Guide safe editing practices per the AGENTS.md guidelines

Repository context:
{json.dumps({k: v[:500] + '...' if len(v) > 500 else v for k, v in self.context.items()}, indent=2)}

Key patterns to follow:
- Excel-driven configs: *_HCS.xlsx, XCH_SCS.xlsx files
- Job numbers: S2#### format
- Template prefixes: HUD_, ZST_, AXC_VAULT
- C# solution: macros/csharp/Solidworks-Automation/Solidworks Automation.sln
- Setup scripts: scripts/setup/SETUP_*.ps1

Safety rules:
- Never suggest modifying binary template files in templates/ or output/
- Flag Excel schema changes for human review
- Recommend testing with SolidWorks when needed

Be concise, helpful, and always reference actual files/patterns from the repository."""
        
        self.messages = [{"role": "system", "content": system_message}]
    
    def ask(self, question: str) -> str:
        """
        Ask a question about the repository.
        
        Args:
            question: The question to ask
            
        Returns:
            The assistant's response
        """
        # Add user message
        self.messages.append({"role": "user", "content": question})
        
        try:
            # Call OpenAI API
            response = self.client.chat.completions.create(
                model=self.model,
                messages=self.messages,
                max_tokens=int(os.getenv('OPENAI_MAX_TOKENS', 2000)),
                temperature=0.7
            )
            
            # Extract response
            answer = response.choices[0].message.content
            
            # Add to conversation history
            self.messages.append({"role": "assistant", "content": answer})
            
            return answer
            
        except Exception as e:
            return f"Error calling OpenAI API: {e}\n\nCheck your .env file and API key."
    
    def interactive_mode(self):
        """Run the assistant in interactive mode."""
        print("=" * 60)
        print("SolidWorks Automation Repository Assistant")
        print("=" * 60)
        print("\nAsk questions about the repository, code patterns, or workflows.")
        print("Type 'exit' or 'quit' to end the session.\n")
        
        while True:
            try:
                question = input("\n?? You: ").strip()
                
                if question.lower() in ['exit', 'quit', 'q']:
                    print("\n?? Goodbye!")
                    break
                
                if not question:
                    continue
                
                print("\n?? Assistant: ", end='')
                answer = self.ask(question)
                print(answer)
                
            except KeyboardInterrupt:
                print("\n\n?? Goodbye!")
                break
            except Exception as e:
                print(f"\n? Error: {e}")


def main():
    """Main entry point."""
    try:
        assistant = RepoAssistant()
        
        # If question provided as argument, answer and exit
        if len(sys.argv) > 1:
            question = ' '.join(sys.argv[1:])
            print(f"?? Question: {question}\n")
            print(f"?? Answer:\n{assistant.ask(question)}\n")
        else:
            # Run in interactive mode
            assistant.interactive_mode()
    
    except ValueError as e:
        print(f"\n? Configuration Error:\n{e}\n")
        sys.exit(1)
    except Exception as e:
        print(f"\n? Unexpected Error: {e}\n")
        import traceback
        traceback.print_exc()
        sys.exit(1)


if __name__ == '__main__':
    main()

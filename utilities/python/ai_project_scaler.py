#!/usr/bin/env python3
"""
AI Project Scaler - Deep Analysis & Continuous Improvement
Uses Perplexity API for real-time web research to scale the project

This tool:
1. Analyzes your entire codebase structure
2. Searches the web for best practices and similar projects
3. Identifies scaling opportunities
4. Provides actionable recommendations
5. Generates implementation roadmaps

Usage:
    python ai_project_scaler.py --analyze-all
    python ai_project_scaler.py --component Bundle --deep
    python ai_project_scaler.py --find-similar-projects
    python ai_project_scaler.py --scaling-strategy
    python ai_project_scaler.py --continuous

Requirements:
    - PERPLEXITY_API_KEY in .env
    - OPENAI_API_KEY in .env (for code analysis)
"""

import os
import sys
import json
import argparse
from pathlib import Path
from datetime import datetime
from typing import List, Dict
import requests
from dotenv import load_dotenv

# Load environment variables from .env file
load_dotenv()

class ProjectScaler:
    """Deep project analysis with web research for scaling strategies"""
    
    def __init__(self):
        # Load API keys
        self.perplexity_key = os.getenv('PERPLEXITY_API_KEY')
        self.openai_key = os.getenv('OPENAI_API_KEY')
        
        if not self.perplexity_key:
            raise ValueError("? PERPLEXITY_API_KEY not found in .env\nGet from: https://www.perplexity.ai/settings/api")
        
        if not self.openai_key:
            print("??  OPENAI_API_KEY not found - some features will be limited")
        
        self.project_root = Path(__file__).parent.parent.parent
        self.code_dir = self.project_root / "macros" / "csharp" / "Solidworks-Automation"
        
        # Initialize OpenAI if available
        if self.openai_key:
            from openai import OpenAI
            self.openai_client = OpenAI(api_key=self.openai_key)
    
    def perplexity_search(self, query: str, search_type: str = "research") -> Dict:
        """
        Search the web using Perplexity API
        
        search_type:
            - "research": Deep research with citations
            - "quick": Fast answers
            - "code": Code-focused search
        """
        
        url = "https://api.perplexity.ai/chat/completions"
        
        # Perplexity model (updated October 2025)
        payload = {
            "model": "sonar",  # Current working model
            "messages": [
                {
                    "role": "system",
                    "content": "You are an expert in CAD automation, SolidWorks API, and software architecture. Provide detailed, actionable insights."
                },
                {
                    "role": "user",
                    "content": query
                }
            ]
        }
        
        headers = {
            "Authorization": f"Bearer {self.perplexity_key}",
            "Content-Type": "application/json"
        }
        
        try:
            response = requests.post(url, json=payload, headers=headers, timeout=60)
            response.raise_for_status()
            return response.json()
        except requests.exceptions.RequestException as e:
            print(f"? Perplexity API error: {e}")
            return {"error": str(e)}
    
    def analyze_project_structure(self) -> Dict:
        """Analyze current project structure"""
        
        structure = {
            "total_files": 0,
            "components": {},
            "total_lines": 0,
            "technologies": set(),
            "patterns": set()
        }
        
        if not self.code_dir.exists():
            return structure
        
        # Scan all C# projects
        for proj_dir in self.code_dir.iterdir():
            if not proj_dir.is_dir() or proj_dir.name.startswith('.'):
                continue
            
            cs_files = list(proj_dir.rglob("*.cs"))
            if not cs_files:
                continue
            
            total_lines = 0
            for cs_file in cs_files:
                try:
                    lines = len(cs_file.read_text(encoding='utf-8').splitlines())
                    total_lines += lines
                except:
                    pass
            
            structure["components"][proj_dir.name] = {
                "files": len(cs_files),
                "lines": total_lines,
                "path": str(proj_dir.relative_to(self.project_root))
            }
            
            structure["total_files"] += len(cs_files)
            structure["total_lines"] += total_lines
        
        # Detect technologies
        if (self.code_dir / "UnifiedUI").exists():
            structure["technologies"].add("WPF/MVVM")
        if any((self.code_dir / comp).exists() for comp in ["Bundle", "Header", "Hood"]):
            structure["technologies"].add("SolidWorks API")
        if (self.code_dir / "Excel").exists():
            structure["technologies"].add("Excel Interop")
        
        # Convert set to list for JSON serialization
        structure["technologies"] = list(structure["technologies"])
        structure["patterns"] = list(structure["patterns"])
        
        return structure
    
    def search_scaling_strategies(self) -> Dict:
        """Search web for CAD automation scaling strategies"""
        
        print("\n?? Searching web for CAD automation scaling strategies...")
        
        query = """
        I have a SolidWorks API automation project with:
        - 22 C# projects generating CAD assemblies
        - 7 component types (Bundle, Header, Hood, Structure, etc.)
        - 50,000+ lines of C#
        - Excel-driven configuration
        - WPF/MVVM UI
        - COM interop with SolidWorks
        
        What are the BEST strategies to scale this to:
        1. 50+ component types
        2. Faster generation (10x speed)
        3. Better maintainability
        4. Automated testing
        5. Cloud deployment potential
        
        Find real-world examples of large-scale CAD automation systems.
        Include specific technologies, architectures, and GitHub projects.
        """
        
        result = self.perplexity_search(query, "research")
        return result
    
    def find_similar_projects(self) -> Dict:
        """Find similar open-source CAD automation projects"""
        
        print("\n?? Finding similar successful CAD automation projects on GitHub...")
        
        query = """
        Find the TOP 10 open-source CAD automation projects on GitHub that use:
        - SolidWorks API or similar (Inventor, CATIA, Fusion 360 APIs)
        - Programmatic CAD generation
        - Multiple component types
        - Enterprise-scale (10,000+ lines)
        
        For each project, provide:
        1. GitHub URL
        2. Stars/popularity
        3. Key features we can learn from
        4. Architecture patterns used
        5. Testing strategies
        6. What makes it successful
        
        Focus on projects that are actively maintained (updated in last 6 months).
        """
        
        result = self.perplexity_search(query, "code")
        return result
    
    def research_performance_optimization(self) -> Dict:
        """Research latest SolidWorks API performance optimization techniques"""
        
        print("\n?? Researching SolidWorks API performance optimizations...")
        
        query = """
        What are the LATEST (2024-2025) performance optimization techniques for SolidWorks API automation?
        
        Focus on:
        1. Batch operations vs individual operations
        2. COM object management best practices
        3. Memory leak prevention
        4. Multi-threading with SolidWorks API
        5. Caching strategies
        6. Template vs code-driven generation
        7. SolidWorks configuration optimization
        8. API call batching
        
        Include code examples and benchmark comparisons if available.
        Cite SolidWorks API documentation and expert sources.
        """
        
        result = self.perplexity_search(query, "research")
        return result
    
    def research_testing_strategies(self) -> Dict:
        """Research CAD automation testing strategies"""
        
        print("\n?? Researching CAD automation testing strategies...")
        
        query = """
        How do enterprise companies test CAD automation systems?
        
        For a SolidWorks API automation project with 50+ components:
        1. Unit testing strategies (mocking COM objects?)
        2. Integration testing with SolidWorks running
        3. Visual regression testing (compare CAD outputs)
        4. Performance testing
        5. CI/CD pipelines for CAD automation
        6. Automated smoke testing
        
        Find real examples from companies like Boeing, Tesla, Caterpillar, etc.
        Include tools, frameworks, and GitHub Actions workflows.
        """
        
        result = self.perplexity_search(query, "research")
        return result
    
    def research_modern_architecture(self) -> Dict:
        """Research modern architecture patterns for CAD automation"""
        
        print("\n?? Researching modern CAD automation architectures...")
        
        query = """
        What are the BEST modern software architecture patterns for large-scale CAD automation?
        
        Topics:
        1. Microservices vs monolithic for CAD generation
        2. Event-driven architecture for component dependencies
        3. CQRS pattern for CAD operations
        4. Plugin architecture for extensibility
        5. API-first design for remote generation
        6. Domain-driven design for engineering components
        
        Find examples from Autodesk Forge, Onshape API, or similar platforms.
        How do they structure 100+ component types?
        """
        
        result = self.perplexity_search(query, "research")
        return result
    
    def generate_scaling_roadmap(self, analyses: Dict) -> str:
        """Generate a comprehensive scaling roadmap from all research"""
        
        if not self.openai_key:
            return "??  OpenAI API key required for roadmap generation"
        
        print("\n?? Generating comprehensive scaling roadmap with AI...")
        
        prompt = f"""Based on this comprehensive research, create a DETAILED scaling roadmap:
        
        PROJECT STRUCTURE:
        {json.dumps(analyses['structure'], indent=2)}
        
        SCALING STRATEGIES RESEARCH:
        {analyses.get('scaling_strategies', {}).get('choices', [{}])[0].get('message', {}).get('content', 'N/A')[:3000]}
        
        SIMILAR PROJECTS FOUND:
        {analyses.get('similar_projects', {}).get('choices', [{}])[0].get('message', {}).get('content', 'N/A')[:3000]}
        
        PERFORMANCE OPTIMIZATIONS:
        {analyses.get('performance', {}).get('choices', [{}])[0].get('message', {}).get('content', 'N/A')[:3000]}
        
        TESTING STRATEGIES:
        {analyses.get('testing', {}).get('choices', [{}])[0].get('message', {}).get('content', 'N/A')[:3000]}
        
        ARCHITECTURE PATTERNS:
        {analyses.get('architecture', {}).get('choices', [{}])[0].get('message', {}).get('content', 'N/A')[:3000]}
        
        Generate a comprehensive scaling roadmap in markdown format with:
        
        # SOLIDWORKS AUTOMATION - SCALING ROADMAP
        
        ## ?? Current State Assessment
        
        ## ?? Scaling Goals (1 year)
        
        ## ?? PHASE 1: Quick Wins (Weeks 1-4)
        ### Immediate improvements with high ROI
        - [ ] Specific task 1 (Why: reason, Impact: X%)
        - [ ] Specific task 2
        
        ## ??? PHASE 2: Foundation (Months 2-3)
        ### Architecture & infrastructure improvements
        
        ## ?? PHASE 3: Scale (Months 4-6)
        ### Scale to 50+ components
        
        ## ?? PHASE 4: Advanced (Months 7-12)
        ### Advanced features & optimization
        
        ## ??? Tools & Technologies to Adopt
        
        ## ?? Learning Resources
        
        ## ?? Success Metrics
        
        ## ?? Risks & Mitigation
        
        Make every recommendation SPECIFIC and ACTIONABLE with WHY and EXPECTED IMPACT.
        """
        
        response = self.openai_client.chat.completions.create(
            model="gpt-4-turbo-preview",
            messages=[
                {"role": "system", "content": "You are a software architecture expert specializing in scaling CAD automation systems."},
                {"role": "user", "content": prompt}
            ],
            temperature=0.3,
            max_tokens=4000
        )
        
        return response.choices[0].message.content
    
    def save_report(self, content: str, report_name: str) -> Path:
        """Save analysis report"""
        
        reports_dir = self.project_root / "docs" / "Architecture"
        reports_dir.mkdir(parents=True, exist_ok=True)
        
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        filename = f"{report_name}_{timestamp}.md"
        output_file = reports_dir / filename
        
        header = f"""# {report_name.replace('_', ' ').title()}

**Generated**: {datetime.now().strftime("%Y-%m-%d %H:%M:%S")}  
**Tool**: AI Project Scaler (Perplexity + OpenAI)

---

"""
        
        output_file.write_text(header + content, encoding='utf-8')
        return output_file
    
    def run_full_analysis(self):
        """Run complete deep analysis with web research"""
        
        print(f"\n{'='*70}")
        print(f"?? DEEP PROJECT ANALYSIS - AI-POWERED SCALING STRATEGY")
        print(f"{'='*70}\n")
        print("This will search the web for:")
        print("  ? Latest CAD automation best practices")
        print("  ? Similar successful projects")
        print("  ? Performance optimization techniques")
        print("  ? Testing strategies")
        print("  ? Modern architecture patterns")
        print("\n??  Estimated time: 3-5 minutes\n")
        
        analyses = {}
        
        # 1. Analyze current project
        print("?? [1/6] Analyzing project structure...")
        analyses['structure'] = self.analyze_project_structure()
        print(f"    Found: {len(analyses['structure']['components'])} components, {analyses['structure']['total_lines']:,} lines")
        
        # 2. Search scaling strategies
        print("\n?? [2/6] Searching web for scaling strategies...")
        analyses['scaling_strategies'] = self.search_scaling_strategies()
        print("    ? Research complete")
        
        # 3. Find similar projects
        print("\n?? [3/6] Finding similar successful projects...")
        analyses['similar_projects'] = self.find_similar_projects()
        print("    ? Projects found")
        
        # 4. Research performance
        print("\n?? [4/6] Researching performance optimizations...")
        analyses['performance'] = self.research_performance_optimization()
        print("    ? Optimizations identified")
        
        # 5. Research testing
        print("\n?? [5/6] Researching testing strategies...")
        analyses['testing'] = self.research_testing_strategies()
        print("    ? Testing strategies found")
        
        # 6. Research architecture
        print("\n?? [6/6] Researching modern architectures...")
        analyses['architecture'] = self.research_modern_architecture()
        print("    ? Architecture patterns identified")
        
        # Generate comprehensive roadmap
        print("\n?? Generating comprehensive scaling roadmap...")
        roadmap = self.generate_scaling_roadmap(analyses)
        
        # Save reports
        print("\n?? Saving reports...")
        
        # Save comprehensive roadmap
        roadmap_file = self.save_report(roadmap, "SCALING_ROADMAP")
        print(f"    ? {roadmap_file.relative_to(self.project_root)}")
        
        # Save detailed research (all findings)
        detailed_report = self._format_detailed_report(analyses)
        detailed_file = self.save_report(detailed_report, "DETAILED_RESEARCH")
        print(f"    ? {detailed_file.relative_to(self.project_root)}")
        
        # Summary
        print(f"\n{'='*70}")
        print(f"? ANALYSIS COMPLETE!")
        print(f"{'='*70}\n")
        print(f"?? Reports generated:")
        print(f"   1. {roadmap_file.name}")
        print(f"   2. {detailed_file.name}")
        print(f"\n?? Next steps:")
        print(f"   1. Read: docs/Architecture/{roadmap_file.name}")
        print(f"   2. Prioritize Phase 1 tasks")
        print(f"   3. Re-run monthly for continuous improvement")
        print()
    
    def _format_detailed_report(self, analyses: Dict) -> str:
        """Format detailed research findings"""
        
        report = "# Detailed Research Findings\n\n"
        
        # Project structure
        report += "## Current Project Structure\n\n"
        report += "```json\n"
        report += json.dumps(analyses['structure'], indent=2)
        report += "\n```\n\n"
        
        # Each research section
        sections = [
            ('scaling_strategies', 'Scaling Strategies'),
            ('similar_projects', 'Similar Projects'),
            ('performance', 'Performance Optimizations'),
            ('testing', 'Testing Strategies'),
            ('architecture', 'Architecture Patterns')
        ]
        
        for key, title in sections:
            report += f"## {title}\n\n"
            if key in analyses and 'choices' in analyses[key]:
                content = analyses[key]['choices'][0]['message']['content']
                report += content + "\n\n"
                
                # Add related questions if available
                if 'related_questions' in analyses[key]['choices'][0]:
                    report += "### Related Questions:\n\n"
                    for q in analyses[key]['choices'][0].get('related_questions', []):
                        report += f"- {q}\n"
                    report += "\n"
            report += "---\n\n"
        
        return report


def main():
    parser = argparse.ArgumentParser(description='AI-powered project scaler with web research')
    parser.add_argument('--analyze-all', action='store_true', help='Run full deep analysis (recommended)')
    parser.add_argument('--scaling-strategy', action='store_true', help='Search for scaling strategies only')
    parser.add_argument('--find-similar-projects', action='store_true', help='Find similar projects only')
    parser.add_argument('--performance', action='store_true', help='Research performance optimizations')
    parser.add_argument('--testing', action='store_true', help='Research testing strategies')
    parser.add_argument('--architecture', action='store_true', help='Research architecture patterns')
    
    args = parser.parse_args()
    
    try:
        scaler = ProjectScaler()
        
        if args.analyze_all or not any([args.scaling_strategy, args.find_similar_projects, 
                                         args.performance, args.testing, args.architecture]):
            # Default: run full analysis
            scaler.run_full_analysis()
        
        elif args.scaling_strategy:
            result = scaler.search_scaling_strategies()
            if 'choices' in result:
                print("\n" + result['choices'][0]['message']['content'])
        
        elif args.find_similar_projects:
            result = scaler.find_similar_projects()
            if 'choices' in result:
                print("\n" + result['choices'][0]['message']['content'])
        
        elif args.performance:
            result = scaler.research_performance_optimization()
            if 'choices' in result:
                print("\n" + result['choices'][0]['message']['content'])
        
        elif args.testing:
            result = scaler.research_testing_strategies()
            if 'choices' in result:
                print("\n" + result['choices'][0]['message']['content'])
        
        elif args.architecture:
            result = scaler.research_modern_architecture()
            if 'choices' in result:
                print("\n" + result['choices'][0]['message']['content'])
    
    except Exception as e:
        print(f"\n? Error: {e}\n")
        import traceback
        traceback.print_exc()
        sys.exit(1)


if __name__ == "__main__":
    main()


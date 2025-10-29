# ?? OpenAI Integration - Quick Start

## ?? URGENT: Security Action Required!

**The API key you shared has been exposed publicly and MUST be revoked immediately!**

### Revoke Now (2 minutes):
1. Go to: https://platform.openai.com/api-keys
2. Find key: `sk-proj-MJTm...` (the one you shared)
3. Click delete/revoke
4. Create a NEW key (you'll use this below)

---

## ?? One-Command Setup

Run this from the repository root:

```powershell
.\SETUP_OPENAI.ps1
```

This will:
1. Create `.env` file
2. Prompt for your NEW API key
3. Install Python dependencies
4. Show you example commands

---

## ?? Manual Setup (If You Prefer)

### Step 1: Create .env file
```powershell
Copy-Item .env.example .env
notepad .env
```

### Step 2: Add Your NEW API Key
Replace `your-openai-api-key-here` with your actual key:
```
OPENAI_API_KEY=sk-proj-YOUR-NEW-KEY-HERE
```

Save and close.

### Step 3: Install Dependencies
```powershell
pip install -r requirements.txt
```

---

## ? Test It Works

### Test 1: Ask a question
```powershell
python utilities\python\ai_repo_assistant.py "What is this repository?"
```

### Test 2: Validate Excel
```powershell
python utilities\python\ai_excel_validator.py "templates\header_section_tool\Combined_\Drafting\Headers\000000_S01c-HCS.xlsx"
```

### Test 3: Interactive chat
```powershell
python utilities\python\ai_repo_assistant.py
```

---

## ??? What You Just Got

### 1. AI Repository Assistant (`ai_repo_assistant.py`)
**What it does:**
- Answers questions about the codebase
- Suggests code patterns
- Finds files and examples
- Guides you through workflows

**Examples:**
```powershell
# Single questions
python utilities\python\ai_repo_assistant.py "How do I build the C# add-in?"
python utilities\python\ai_repo_assistant.py "Where are job number S2045 files?"
python utilities\python\ai_repo_assistant.py "Explain the Header Section Tool"

# Interactive mode (back-and-forth conversation)
python utilities\python\ai_repo_assistant.py
```

### 2. Excel Validator (`ai_excel_validator.py`)
**What it does:**
- Analyzes Excel config files
- Detects breaking changes
- Lists C# files that need updates
- Provides safety checklist

**Examples:**
```powershell
# Validate current file
python utilities\python\ai_excel_validator.py "templates\header_section_tool\Single_\Drafting\Headers\000000_S03-HCS.xlsx"

# Compare old vs new
python utilities\python\ai_excel_validator.py "backup\old_HCS.xlsx" "templates\header_section_tool\Combined_\Drafting\Headers\000000_S01c-HCS.xlsx"
```

### 3. Excel Summary (`excel_summary.py`)
**What it does:**
- Quick preview of Excel structure
- No AI needed (works offline)

---

## ?? Costs

**Typical costs with GPT-4 Turbo:**
- Simple question: $0.01-0.03
- Excel validation: $0.05-0.10
- Long conversation: $0.20-0.50

**Set limits:** https://platform.openai.com/account/billing/limits

---

## ?? Security Checklist

- [x] `.env` added to `.gitignore` (already done)
- [ ] Revoked the exposed API key
- [ ] Created new API key
- [ ] Added new key to `.env` file
- [ ] Never share `.env` file
- [ ] Never commit API keys to Git

---

## ?? Full Documentation

- **Setup Guide**: `utilities/python/OPENAI_SETUP.md`
- **Python Utilities**: `utilities/python/README.md`
- **Agent Guide**: `AGENTS.md` or `.github/AGENTS.md`

---

## ?? Troubleshooting

**"OpenAI API key not found"**
- Run: `.\SETUP_OPENAI.ps1`
- Or manually edit `.env` file

**"Import 'openai' could not be resolved"**
- Run: `pip install -r requirements.txt`

**"Invalid API key"**
- Get new key from: https://platform.openai.com/api-keys
- Update in `.env` file

---

## ?? Next Steps

1. **Revoke the exposed key** (do this NOW!)
2. **Run setup**: `.\SETUP_OPENAI.ps1`
3. **Try it**: `python utilities\python\ai_repo_assistant.py`
4. **Explore**: Ask questions about the codebase!

Need help? Check `utilities/python/OPENAI_SETUP.md` for detailed troubleshooting.

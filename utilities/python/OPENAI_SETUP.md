# OpenAI Integration Setup Guide

## ?? Security First!

**IMPORTANT**: Your API key from the chat has been exposed and should be revoked immediately!

### Step 1: Revoke the Exposed Key

1. Go to https://platform.openai.com/api-keys
2. Find the key starting with `sk-proj-MJTm...`
3. Click the trash/delete icon or "Revoke" button
4. Confirm deletion

### Step 2: Create a New API Key

1. Still on https://platform.openai.com/api-keys
2. Click "+ Create new secret key"
3. Give it a name (e.g., "SolidWorks Automation")
4. **Copy the key immediately** (you won't see it again!)

### Step 3: Set Up Local Environment

1. Navigate to the repository root in PowerShell:
   ```powershell
   cd C:\Users\DCornealius\CascadeProjects\Solidworks_Automation
   ```

2. Copy the example environment file:
   ```powershell
   Copy-Item .env.example .env
   ```

3. Open `.env` in your editor and add your NEW key:
   ```powershell
   notepad .env
   ```

4. Replace `your-openai-api-key-here` with your actual key:
   ```
   OPENAI_API_KEY=sk-proj-YOUR-NEW-KEY-HERE
   OPENAI_MODEL=gpt-4-turbo-preview
   ```

5. Save and close the file

### Step 4: Install Python Dependencies

```powershell
pip install -r requirements.txt
```

This installs:
- `openai` - OpenAI API client
- `python-dotenv` - Environment variable management
- `PyGithub` - GitHub API integration
- `gitpython` - Git operations

## ? Test Your Setup

### Test 1: AI Repository Assistant

```powershell
python utilities\python\ai_repo_assistant.py "What is this repository?"
```

Expected: AI responds with a summary of the SolidWorks automation project.

### Test 2: Excel Validator

```powershell
python utilities\python\ai_excel_validator.py "templates\header_section_tool\Combined_\Drafting\Headers\000000_S01c-HCS.xlsx"
```

Expected: AI analyzes the Excel file and provides schema summary and safety checklist.

### Test 3: Interactive Mode

```powershell
python utilities\python\ai_repo_assistant.py
```

Then ask questions like:
- "How do I build the add-in?"
- "Where are the setup scripts?"
- "What's the job number format?"

## ??? Security Best Practices

### DO:
? Keep `.env` file local (it's in `.gitignore`)
? Use different API keys for dev/prod
? Set usage limits in OpenAI dashboard
? Revoke keys immediately if exposed

### DON'T:
? Commit `.env` to Git
? Share API keys in chat/email/Slack
? Hard-code keys in source files
? Push keys to GitHub (even in "private" repos)

## ?? What You Can Do Now

### 1. Ask Questions About the Codebase
```powershell
python utilities\python\ai_repo_assistant.py
```

Try:
- "Explain the Excel-driven tools architecture"
- "How do I add a new automation module?"
- "What's the difference between Header Section Tool variants?"

### 2. Validate Excel Changes Before Committing
```powershell
python utilities\python\ai_excel_validator.py "path\to\modified.xlsx"
```

Gets:
- Breaking change warnings
- Required code updates
- Documentation update checklist

### 3. Future Enhancements (Coming Soon)

We can add:
- **PR Review Bot**: Automatically reviews pull requests using AI
- **Code Generator**: Creates boilerplate following project patterns
- **Doc Generator**: Auto-updates integration guides
- **Smart Search**: Natural language search across all files

## ?? Cost Management

### Typical Usage Costs (with GPT-4 Turbo):
- Simple question: ~$0.01-0.03
- Excel validation: ~$0.05-0.10
- Long conversation: ~$0.20-0.50

### Set Spending Limits:
1. Go to https://platform.openai.com/account/billing/limits
2. Set hard limit (e.g., $10/month)
3. Set soft limit (e.g., $5 = email alert)

### Monitor Usage:
https://platform.openai.com/usage

## ?? Troubleshooting

### "OpenAI API key not found"
- Check `.env` file exists in repository root
- Verify `OPENAI_API_KEY=` line has your actual key (no quotes needed)
- Ensure no extra spaces around the `=`

### "Invalid API key"
- Key might be revoked - create a new one
- Check for copy/paste errors (key should start with `sk-`)
- Verify key is active at https://platform.openai.com/api-keys

### "Import 'openai' could not be resolved"
- Run: `pip install -r requirements.txt`
- Verify Python environment is activated
- Check you're in the repo root directory

### Rate Limit Errors
- You've hit OpenAI's rate limit
- Wait a few seconds and try again
- Consider upgrading your OpenAI plan

## ?? Support

- OpenAI API Docs: https://platform.openai.com/docs
- OpenAI Help: https://help.openai.com
- Repository Issues: https://github.com/swiffc/codestack/issues

---

**Remember**: Never share your API key! Treat it like a password. ??

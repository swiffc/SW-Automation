# Job & Drawing Browser Integration

**Purpose**: Easy access to job files in AXC_VAULT Active folder

---

## ?? Overview

Your vault structure:
```
C:\AXC_VAULT\Active\
??? S2XXXX\
?   ??? Drafting\
?       ??? Certified\
?       ?   ??? S2XXXX-6A.slddrw
?       ??? [other folders]
??? S24462\
??? S24463\
??? S2XXXX\ (hundreds of jobs)
```

**Need**: Quick way to find and open job drawings and files

---

## ?? Solution: Multi-Level Access System

### Level 1: Quick Job Launcher (Task Pane)
**Best For**: Daily use, recent jobs

### Level 2: Job Search & Browser (Dialog)
**Best For**: Finding specific jobs

### Level 3: File Explorer Integration
**Best For**: Advanced browsing

---

## ?? Job Vault Structure Mapping

```json
{
  "VaultStructure": {
    "ActiveJobsPath": "C:\\AXC_VAULT\\Active",
    "JobPattern": "S2####",
    "StandardFolders": {
      "Drafting": {
        "Certified": "Production-ready drawings",
        "WIP": "Work in progress",
        "Archive": "Historical files"
      },
      "Models": {
        "Assemblies": "*.SLDASM",
        "Parts": "*.SLDPRT"
      },
      "Documentation": {
        "PDF": "*.pdf",
        "Excel": "*.xlsx, *.xls"
      }
    }
  }
}
```

---

## ?? UI Design

### Task Pane: "Job Browser"

```
???????????????????????????????????
?  ?? Job Browser                 ?
???????????????????????????????????
?  Search: [S2XXXX________] ??    ?
???????????????????????????????????
?  ?? Favorites                   ?
?    • S2XXXX - XYZ Project       ?
?    • S24320 - ABC Project       ?
?    • S24105 - DEF Project       ?
???????????????????????????????????
?  ?? Recent Jobs                 ?
?    • S2XXXX (Today, 2:30 PM)    ?
?    • S24455 (Today, 10:15 AM)   ?
?    • S24442 (Yesterday)         ?
???????????????????????????????????
?  ?? Quick Actions               ?
?    [Open Certified Folder]      ?
?    [Browse All Jobs]            ?
?    [Advanced Search]            ?
???????????????????????????????????
```

### Dialog: "Advanced Job Browser"

```
?????????????????????????????????????????????????????????????????
?  Job & Drawing Browser                              [_][?][X]  ?
?????????????????????????????????????????????????????????????????
?  Search: [S24___] ??  Filter: [All Files ?]  [Search]          ?
?????????????????????????????????????????????????????????????????
?  Jobs (125 found)    ?  Files in S2XXXX\Drafting\Certified    ?
?                      ?                                        ?
?  ?? S2XXXX           ?  ?? S2XXXX-6A.slddrw      Modified: ... ?
?  ?? S24460           ?  ?? S2XXXX-6B.slddrw      Modified: ... ?
?  ?? S24459           ?  ?? S2XXXX-7.sldasm       Modified: ... ?
?  ?? S24458           ?  ?? S2XXXX-Details.pdf    Modified: ... ?
?  ?? S24457           ?  ?? S2XXXX-BOM.xlsx       Modified: ... ?
?  ?? S24456           ?                                        ?
?  ...                 ?  [Open]  [Open Folder]  [Add Favorite] ?
?????????????????????????????????????????????????????????????????
```

---

## ?? Implementation Options

### Option A: Standalone WPF Tool (Recommended)
**Pros**:
- ? Fast development
- ? Works outside SolidWorks
- ? Can be launched independently
- ? Rich UI capabilities

**Use**: Desktop app for browsing vault

### Option B: SolidWorks Add-In Integration
**Pros**:
- ? Integrated in SolidWorks
- ? Direct file opening
- ? Task pane always available

**Use**: Add to your existing Solidworks-Automation add-in

### Option C: Hybrid (Best of Both) ?
**Combine both**:
- Desktop tool for browsing/searching
- Task pane in SolidWorks for quick access
- Share recent files list between them

---

## ?? Features to Implement

### Core Features (Must Have)

1. **Job Search**
   - Search by job number (S2####)
   - Wildcard support (S244*)
   - Fuzzy search
   - Quick jump (type S2XXXX, press Enter)

2. **File Browsing**
   - List all files in job's Drafting\Certified folder
   - Show file types, dates, sizes
   - Preview thumbnails (if possible)
   - Double-click to open

3. **Recent Jobs**
   - Track last 20 opened jobs
   - Show date/time accessed
   - Quick re-open
   - Persist across sessions

4. **Favorites**
   - Bookmark frequently-used jobs
   - Add custom notes
   - Organize in folders
   - Export/import favorites

### Advanced Features (Nice to Have)

5. **Smart Filters**
   - By file type (.slddrw, .sldasm, .pdf, etc.)
   - By date (today, this week, this month)
   - By status (WIP, Certified, Archive)
   - By custom tags

6. **Quick Actions**
   - Open in SolidWorks
   - Open folder in Explorer
   - Copy file path
   - Open related files
   - Export to PDF (if drawing)

7. **Integration**
   - Open from Windows Explorer context menu
   - Launch from Windows search
   - Pin to Windows taskbar
   - Command-line support

8. **Statistics**
   - Most used jobs
   - Recently modified files
   - Job activity tracking

---

## ??? Configuration File

```json
{
  "JobBrowser": {
    "VaultPath": "C:\\AXC_VAULT\\Active",
    "JobPattern": "S2\\d{4}",
    "StandardFolders": [
      "Drafting\\Certified",
      "Drafting\\WIP",
      "Drafting\\Archive",
      "Models\\Assemblies",
      "Models\\Parts"
    ],
    
    "FileTypes": {
      "Drawings": [".slddrw"],
      "Assemblies": [".sldasm"],
      "Parts": [".sldprt"],
      "Documents": [".pdf", ".docx"],
      "Spreadsheets": [".xlsx", ".xls"],
      "Images": [".jpg", ".png", ".bmp"],
      "All": ["*"]
    },
    
    "DefaultFolder": "Drafting\\Certified",
    "ShowHiddenFiles": false,
    "CacheJobList": true,
    "CacheDuration": 3600,
    "MaxRecentJobs": 20,
    "MaxFavorites": 50,
    
    "QuickOpen": {
      "DoubleClickAction": "OpenInSolidWorks",
      "MiddleClickAction": "OpenFolder",
      "CtrlClickAction": "OpenInNewWindow"
    },
    
    "Search": {
      "EnableFuzzySearch": true,
      "EnableWildcards": true,
      "SearchInSubfolders": true,
      "IndexFiles": true
    }
  },
  
  "RecentJobs": {
    "FilePath": "data\\recent_jobs.json",
    "AutoSave": true
  },
  
  "Favorites": {
    "FilePath": "data\\favorites.json",
    "AutoBackup": true
  }
}
```

---

## ?? Code Structure

```
JobBrowser/
??? Core/
?   ??? VaultScanner.cs          # Scans Active folder for jobs
?   ??? JobInfo.cs               # Represents a job
?   ??? FileInfo.cs              # Represents a file
?   ??? SearchEngine.cs          # Search logic
?   ??? CacheManager.cs          # Performance caching
?
??? Data/
?   ??? RecentJobsManager.cs     # Recent jobs tracking
?   ??? FavoritesManager.cs      # Favorites management
?   ??? JobDatabase.cs           # In-memory job database
?
??? UI/
?   ??? TaskPane/
?   ?   ??? JobBrowserTaskPane.xaml
?   ??? Dialogs/
?   ?   ??? AdvancedSearchDialog.xaml
?   ?   ??? JobDetailsDialog.xaml
?   ??? Controls/
?       ??? JobListControl.xaml
?       ??? FileListControl.xaml
?
??? Integration/
?   ??? SolidWorksIntegration.cs # Open files in SW
?   ??? ExplorerIntegration.cs   # Windows Explorer
?   ??? CommandLine.cs           # CLI support
?
??? Config/
    ??? JobBrowserConfig.cs      # Configuration
```

---

## ?? Quick Implementation

### Step 1: Create VaultScanner

```csharp
public class VaultScanner
{
    private string _vaultPath = @"C:\AXC_VAULT\Active";
    
    public List<JobInfo> ScanJobs(string searchPattern = null)
    {
        var jobs = new List<JobInfo>();
        
        var directories = Directory.GetDirectories(_vaultPath, "S2*");
        
        foreach (var dir in directories)
        {
            var jobNumber = Path.GetFileName(dir);
            
            if (searchPattern == null || jobNumber.Contains(searchPattern))
            {
                var jobInfo = new JobInfo
                {
                    JobNumber = jobNumber,
                    Path = dir,
                    DraftingPath = Path.Combine(dir, "Drafting", "Certified"),
                    Files = ScanFiles(Path.Combine(dir, "Drafting", "Certified"))
                };
                
                jobs.Add(jobInfo);
            }
        }
        
        return jobs.OrderByDescending(j => j.JobNumber).ToList();
    }
    
    private List<FileInfo> ScanFiles(string path)
    {
        if (!Directory.Exists(path))
            return new List<FileInfo>();
        
        var files = new List<FileInfo>();
        
        var drawingFiles = Directory.GetFiles(path, "*.slddrw");
        var assemblyFiles = Directory.GetFiles(path, "*.sldasm");
        var partFiles = Directory.GetFiles(path, "*.sldprt");
        var pdfFiles = Directory.GetFiles(path, "*.pdf");
        
        foreach (var file in drawingFiles.Concat(assemblyFiles)
                                        .Concat(partFiles)
                                        .Concat(pdfFiles))
        {
            var fileInfo = new FileInfo
            {
                Name = Path.GetFileName(file),
                FullPath = file,
                Extension = Path.GetExtension(file),
                Size = new System.IO.FileInfo(file).Length,
                ModifiedDate = File.GetLastWriteTime(file)
            };
            
            files.Add(fileInfo);
        }
        
        return files.OrderBy(f => f.Name).ToList();
    }
}

public class JobInfo
{
    public string JobNumber { get; set; }
    public string Path { get; set; }
    public string DraftingPath { get; set; }
    public List<FileInfo> Files { get; set; }
    public bool IsFavorite { get; set; }
}

public class FileInfo
{
    public string Name { get; set; }
    public string FullPath { get; set; }
    public string Extension { get; set; }
    public long Size { get; set; }
    public DateTime ModifiedDate { get; set; }
}
```

### Step 2: Create Simple WPF Browser

```csharp
public partial class JobBrowserWindow : Window
{
    private VaultScanner _scanner = new VaultScanner();
    private List<JobInfo> _allJobs;
    
    public JobBrowserWindow()
    {
        InitializeComponent();
        LoadJobs();
    }
    
    private void LoadJobs()
    {
        // Show loading indicator
        StatusText.Text = "Scanning vault...";
        
        Task.Run(() =>
        {
            _allJobs = _scanner.ScanJobs();
            
            Dispatcher.Invoke(() =>
            {
                JobListBox.ItemsSource = _allJobs;
                StatusText.Text = $"Found {_allJobs.Count} jobs";
            });
        });
    }
    
    private void SearchButton_Click(object sender, RoutedEventArgs e)
    {
        var searchText = SearchTextBox.Text;
        
        var filteredJobs = _allJobs.Where(j => 
            j.JobNumber.Contains(searchText, StringComparison.OrdinalIgnoreCase))
            .ToList();
        
        JobListBox.ItemsSource = filteredJobs;
        StatusText.Text = $"Found {filteredJobs.Count} jobs matching '{searchText}'";
    }
    
    private void JobListBox_SelectionChanged(object sender, SelectionChangedEventArgs e)
    {
        if (JobListBox.SelectedItem is JobInfo job)
        {
            FileListBox.ItemsSource = job.Files;
        }
    }
    
    private void FileListBox_MouseDoubleClick(object sender, MouseButtonEventArgs e)
    {
        if (FileListBox.SelectedItem is FileInfo file)
        {
            OpenFileInSolidWorks(file.FullPath);
        }
    }
    
    private void OpenFileInSolidWorks(string filePath)
    {
        try
        {
            // Get SolidWorks instance
            var swApp = (SldWorks)Marshal.GetActiveObject("SldWorks.Application");
            
            // Open the file
            int errors = 0;
            int warnings = 0;
            swApp.OpenDoc6(filePath, (int)swDocumentTypes_e.swDocDRAWING, 
                          (int)swOpenDocOptions_e.swOpenDocOptions_Silent, 
                          "", ref errors, ref warnings);
            
            StatusText.Text = $"Opened: {Path.GetFileName(filePath)}";
        }
        catch (Exception ex)
        {
            MessageBox.Show($"Error opening file: {ex.Message}", "Error", 
                          MessageBoxButton.OK, MessageBoxImage.Error);
        }
    }
}
```

---

## ?? Integration with Your Add-In

### Add to SolidWorks Add-In

```csharp
// In your SolidAddIn class
public override void ApplicationStartup()
{
    // Create Job Browser task pane
    CreateJobBrowserTaskPane();
    
    // Add to command manager
    AddJobBrowserCommand();
}

private void CreateJobBrowserTaskPane()
{
    var taskpane = Taskpane.Create<JobBrowserTaskPaneControl>(
        title: "Job Browser",
        icon: "job_browser_icon.png"
    );
    
    taskpane.Visible = true;
}

private void AddJobBrowserCommand()
{
    var group = CommandManager.GetGroup("Job Tools");
    
    var openJobCommand = group.AddItem<OpenJobCommand>();
    openJobCommand.Title = "Open Job";
    openJobCommand.Tooltip = "Quick open job drawing";
    openJobCommand.OnClick += () =>
    {
        var dialog = new QuickJobOpenDialog();
        dialog.ShowDialog();
    };
}
```

---

## ?? Advanced Features

### Recent Jobs Tracking

```csharp
public class RecentJobsManager
{
    private string _filePath = "data\\recent_jobs.json";
    private List<RecentJob> _recentJobs = new List<RecentJob>();
    
    public void AddRecentJob(string jobNumber, string filePath)
    {
        var recent = new RecentJob
        {
            JobNumber = jobNumber,
            FilePath = filePath,
            AccessedDate = DateTime.Now
        };
        
        // Remove if already exists
        _recentJobs.RemoveAll(r => r.JobNumber == jobNumber);
        
        // Add to beginning
        _recentJobs.Insert(0, recent);
        
        // Keep only last 20
        if (_recentJobs.Count > 20)
            _recentJobs = _recentJobs.Take(20).ToList();
        
        Save();
    }
    
    public List<RecentJob> GetRecentJobs()
    {
        return _recentJobs;
    }
    
    private void Save()
    {
        var json = JsonConvert.SerializeObject(_recentJobs, Formatting.Indented);
        File.WriteAllText(_filePath, json);
    }
    
    public void Load()
    {
        if (File.Exists(_filePath))
        {
            var json = File.ReadAllText(_filePath);
            _recentJobs = JsonConvert.DeserializeObject<List<RecentJob>>(json);
        }
    }
}

public class RecentJob
{
    public string JobNumber { get; set; }
    public string FilePath { get; set; }
    public DateTime AccessedDate { get; set; }
}
```

### Favorites Management

```csharp
public class FavoritesManager
{
    private string _filePath = "data\\favorites.json";
    private List<FavoriteJob> _favorites = new List<FavoriteJob>();
    
    public void AddFavorite(string jobNumber, string notes = "")
    {
        if (!_favorites.Any(f => f.JobNumber == jobNumber))
        {
            _favorites.Add(new FavoriteJob
            {
                JobNumber = jobNumber,
                Notes = notes,
                AddedDate = DateTime.Now
            });
            
            Save();
        }
    }
    
    public void RemoveFavorite(string jobNumber)
    {
        _favorites.RemoveAll(f => f.JobNumber == jobNumber);
        Save();
    }
    
    public List<FavoriteJob> GetFavorites()
    {
        return _favorites.OrderBy(f => f.JobNumber).ToList();
    }
    
    private void Save()
    {
        var json = JsonConvert.SerializeObject(_favorites, Formatting.Indented);
        File.WriteAllText(_filePath, json);
    }
    
    public void Load()
    {
        if (File.Exists(_filePath))
        {
            var json = File.ReadAllText(_filePath);
            _favorites = JsonConvert.DeserializeObject<List<FavoriteJob>>(json);
        }
    }
}

public class FavoriteJob
{
    public string JobNumber { get; set; }
    public string Notes { get; set; }
    public DateTime AddedDate { get; set; }
}
```

---

## ?? Quick Start Implementation

### Minimal Viable Product (1-2 days)

1. **Create Console App** for testing
2. **Implement VaultScanner** class
3. **Test scanning** C:\AXC_VAULT\Active
4. **Create simple WPF window** with job list
5. **Add double-click** to open files

### Full Implementation (1-2 weeks)

1. **Week 1: Core Features**
   - VaultScanner with caching
   - Search functionality
   - File browser
   - SolidWorks integration
   - Recent jobs tracking

2. **Week 2: Advanced Features**
   - Favorites system
   - Task pane integration
   - Advanced filters
   - Performance optimization
   - Error handling

---

## ?? Project Structure

```
Solidworks_Automation/
??? JobBrowser/                  ? NEW PROJECT
?   ??? JobBrowser.csproj
?   ??? Core/
?   ?   ??? VaultScanner.cs
?   ?   ??? SearchEngine.cs
?   ?   ??? Models/
?   ??? UI/
?   ?   ??? MainWindow.xaml
?   ?   ??? TaskPane.xaml
?   ?   ??? Controls/
?   ??? Data/
?   ?   ??? RecentJobsManager.cs
?   ?   ??? FavoritesManager.cs
?   ?   ??? data/
?   ?       ??? recent_jobs.json
?   ?       ??? favorites.json
?   ??? Config/
?       ??? job_browser_config.json
?
??? macros/csharp/Solidworks-Automation/
    ??? (integrate JobBrowser as reference)
```

---

## ? Next Steps

1. **Review this design** - Does it match your needs?
2. **Choose implementation** - Standalone, Add-in, or Hybrid?
3. **Start with MVP** - Basic scanner + file list
4. **Test with real jobs** - Verify vault structure
5. **Add features incrementally**

Would you like me to create the initial implementation code?




using System;
using System.Collections.Generic;

namespace UnifiedUI.Models.Configuration
{
    /// <summary>
    /// Application settings model
    /// </summary>
    public class AppSettings
    {
        public string Version { get; set; }
        public DateTime LastModified { get; set; }
        public PathSettings Paths { get; set; }
        public SolidWorksSettings SolidWorks { get; set; }
        public WorkflowSettings Workflows { get; set; }
        public AnalyticsSettings Analytics { get; set; }
        public UISettings UI { get; set; }
        public PerformanceSettings Performance { get; set; }
        public Dictionary<string, string> UserPreferences { get; set; }

        public AppSettings()
        {
            Version = "1.0.0";
            LastModified = DateTime.Now;
            Paths = new PathSettings();
            SolidWorks = new SolidWorksSettings();
            Workflows = new WorkflowSettings();
            Analytics = new AnalyticsSettings();
            UI = new UISettings();
            Performance = new PerformanceSettings();
            UserPreferences = new Dictionary<string, string>();
        }
    }

    /// <summary>
    /// Path settings for file locations
    /// </summary>
    public class PathSettings
    {
        public string TemplatesPath { get; set; }
        public string WorkflowsPath { get; set; }
        public string OutputPath { get; set; }
        public string LogsPath { get; set; }
        public string BackupPath { get; set; }
        public string TempPath { get; set; }
        public Dictionary<string, string> CustomPaths { get; set; }

        public PathSettings()
        {
            var appDataPath = Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData);
            var basePath = System.IO.Path.Combine(appDataPath, "SolidWorksAutomation");

            TemplatesPath = System.IO.Path.Combine(basePath, "Templates");
            WorkflowsPath = System.IO.Path.Combine(basePath, "Workflows");
            OutputPath = System.IO.Path.Combine(basePath, "Output");
            LogsPath = System.IO.Path.Combine(basePath, "Logs");
            BackupPath = System.IO.Path.Combine(basePath, "Backups");
            TempPath = System.IO.Path.GetTempPath();
            CustomPaths = new Dictionary<string, string>();
        }
    }

    /// <summary>
    /// SolidWorks-specific settings
    /// </summary>
    public class SolidWorksSettings
    {
        public bool AutoStartSolidWorks { get; set; }
        public bool KeepSolidWorksVisible { get; set; }
        public bool ShowProgressDialogs { get; set; }
        public bool AutoSaveOnCompletion { get; set; }
        public bool CloseDocumentsAfterProcessing { get; set; }
        public int DocumentTimeout { get; set; }
        public int RebuildTimeout { get; set; }
        public bool EnableSafeMode { get; set; }
        public string DefaultTemplate { get; set; }
        public bool UseBackgroundProcessing { get; set; }

        public SolidWorksSettings()
        {
            AutoStartSolidWorks = true;
            KeepSolidWorksVisible = true;
            ShowProgressDialogs = true;
            AutoSaveOnCompletion = true;
            CloseDocumentsAfterProcessing = false;
            DocumentTimeout = 300; // 5 minutes
            RebuildTimeout = 600; // 10 minutes
            EnableSafeMode = false;
            UseBackgroundProcessing = false;
        }
    }

    /// <summary>
    /// Workflow execution settings
    /// </summary>
    public class WorkflowSettings
    {
        public bool EnableParallelExecution { get; set; }
        public int MaxParallelWorkflows { get; set; }
        public bool EnableAutoRetry { get; set; }
        public int DefaultRetryCount { get; set; }
        public int DefaultStepTimeout { get; set; }
        public bool SaveWorkflowHistory { get; set; }
        public int MaxHistoryEntries { get; set; }
        public bool EnableWorkflowLogging { get; set; }
        public bool ContinueOnStepFailure { get; set; }

        public WorkflowSettings()
        {
            EnableParallelExecution = false;
            MaxParallelWorkflows = 3;
            EnableAutoRetry = true;
            DefaultRetryCount = 3;
            DefaultStepTimeout = 300;
            SaveWorkflowHistory = true;
            MaxHistoryEntries = 100;
            EnableWorkflowLogging = true;
            ContinueOnStepFailure = false;
        }
    }

    /// <summary>
    /// Analytics and reporting settings
    /// </summary>
    public class AnalyticsSettings
    {
        public bool EnableAnalytics { get; set; }
        public bool TrackWorkflowPerformance { get; set; }
        public bool TrackErrorRates { get; set; }
        public bool GenerateReports { get; set; }
        public string ReportOutputPath { get; set; }
        public int ReportRetentionDays { get; set; }

        public AnalyticsSettings()
        {
            EnableAnalytics = true;
            TrackWorkflowPerformance = true;
            TrackErrorRates = true;
            GenerateReports = true;
            ReportRetentionDays = 90;
        }
    }

    /// <summary>
    /// UI-specific settings
    /// </summary>
    public class UISettings
    {
        public string Theme { get; set; }
        public double WindowWidth { get; set; }
        public double WindowHeight { get; set; }
        public bool RememberWindowPosition { get; set; }
        public bool ShowTooltips { get; set; }
        public bool EnableAnimations { get; set; }
        public int FontSize { get; set; }

        public UISettings()
        {
            Theme = "Light";
            WindowWidth = 1200;
            WindowHeight = 800;
            RememberWindowPosition = true;
            ShowTooltips = true;
            EnableAnimations = true;
            FontSize = 12;
        }
    }

    /// <summary>
    /// Performance and optimization settings
    /// </summary>
    public class PerformanceSettings
    {
        public bool EnableCaching { get; set; }
        public int CacheSizeMB { get; set; }
        public bool OptimizeMemoryUsage { get; set; }
        public bool EnableMultithreading { get; set; }
        public int MaxThreads { get; set; }

        public PerformanceSettings()
        {
            EnableCaching = true;
            CacheSizeMB = 500;
            OptimizeMemoryUsage = true;
            EnableMultithreading = true;
            MaxThreads = Environment.ProcessorCount;
        }
    }
}

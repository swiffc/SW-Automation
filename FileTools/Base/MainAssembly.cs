using System.Collections.Generic;
using System.IO;
using static FileTools.StaticFileTools;
using static Tools.ModelTools;
using static FileTools.CommonData.CommonData;
using FileTools.CommonData;
using static FileTools.Properties.Settings;
using SolidWorks.Interop.sldworks;
using System;
using System.Diagnostics;

namespace FileTools.Base
{
    public class MainAssembly : SW_Assembly
    {
        // Static properties
        static public bool LibraryFilesDownloaded { get; set; } = false;
        static public bool TemplatesDownloaded { get; set; } = false;


        // Constructor
        public MainAssembly(int assemblyNumber, string assemblyDescription, params Type[] classesToIsolate)
        {
            foreach (var type in classesToIsolate)
                ClassesToIsolate.Add(type);

            AssemblyNumber = assemblyNumber;
            AssemblyDesc = assemblyDescription;

            Setup();

            bool bankExists;
            do
            {
                bankExists = File.Exists(AssemblyPath);
                if (bankExists)
                {
                    AssemblyDoc = OpenAssembly(AssemblyPath, AssemblyNumber.ToString(), false);

                    Dimensions();
                    Sketches();

                    var componentList = InstantiateComponents(this);
                    LocateComponents(componentList, this);

                    if (!Developer)
                    {
                        if (Toggle_CreateDrawing)
                            CreateDrawing(componentList, this);

                        if (Toggle_Save && !Toggle_DeleteFiles)
                            SaveEverything();

                        if (Toggle_DeleteFiles)
                        {
                            SaveEverything();
                            Close(AssemblyPath);
                            DeleteUnusedFiles();
                            AssemblyDoc = OpenAssembly(AssemblyPath, AssemblyNumber.ToString(), false);
                        }
                    }
                    ForceRebuild(AssemblyDoc);
                    TurnOffBendLines();
                }
                else
                {
                    Default.Bank = AddNew_Bank();
                }
            } while (!bankExists);
        }
    }
}

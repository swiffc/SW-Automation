using System.Collections.Generic;
using System.IO;
using static FileTools.StaticFileTools;
using static Tools.ModelTools;
using static FileTools.CommonData.CommonData;
using FileTools.CommonData;

namespace FileTools.Base
{
    public class MainAssembly : SW_Assembly
    {
        public MainAssembly(int assemblyNumber, string assemblyDescription)
        {
            AssemblyNumber = assemblyNumber;
            AssemblyDesc = assemblyDescription;

            bool bankExists;
            do
            {
                bankExists = File.Exists(AssemblyPath);
                if (bankExists)
                {
                    AssemblyDoc = OpenAssembly(AssemblyPath, AssemblyNumber.ToString(), false);

                    var componentList = InstantiateComponents(this);
                    LocateComponents(componentList, this);

                    if (ToggleCreateDrawing)
                        CreateDrawing(componentList, this);

                    if (ToggleSave && !ToggleDeleteFiles)
                    {
                        OpenAssembly(AssemblyPath, AssemblyNumber.ToString(), false);
                        SaveEverything();
                    } 

                    if (ToggleDeleteFiles)
                    {
                        OpenAssembly(AssemblyPath, AssemblyNumber.ToString(), false);
                        SaveEverything();
                        CloseEverything();
                        ClearList_ToBeDeleted();
                    }

                    OpenAssembly(AssemblyPath, AssemblyNumber.ToString(), false);
                    Rebuild(true);
                }
                else
                {
                    Bank = AddNew_Bank();
                }
            } while (!bankExists);
        }
    }
}

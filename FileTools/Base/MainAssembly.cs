using System.Collections.Generic;
using System.IO;
using static FileTools.StaticFileTools;
using static Tools.ModelTools;

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
                    CreateDrawing(componentList, this);
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

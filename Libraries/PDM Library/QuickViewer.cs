using LdrEditAssembly;
using SolidWorks.Interop.sldworks;
using System.IO;

namespace PDM_Library
{
    public class QuickViewer
    {
        public static void Main(string[] args)
        {
            ISldWorks sldWorks = new SldWorks();
            foreach (string filePath in args)
            {
                IDocumentSpecification docSpec = sldWorks.GetOpenDocSpec(filePath);
                string fileExt = Path.GetExtension(filePath);
                if (fileExt == ".SLDDRW")
                {
                    docSpec.DetailingMode = true;
                }
                else if (fileExt == ".SLDASM")
                {
                    docSpec.ViewOnly = true;
                }
                else if (fileExt == ".SLDPRT")
                {
                    docSpec.ViewOnly = true;
                }
                IModelDoc2 modelDoc2 = sldWorks.OpenDoc7(docSpec);


                    System.Threading.Thread.Sleep(10000);

                LdrEditAssembly.LdrEditAssembly.EnableEditAssemblyLDR();
            }
        }
    }
}

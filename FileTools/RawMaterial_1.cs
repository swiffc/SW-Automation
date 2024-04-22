using SolidWorks.Interop.sldworks;
using SolidWorks.Interop.swconst;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static FileTools.Base.Part;
using static FileTools.StaticFileTools;

namespace FileTools
{
    public static partial class RawMaterial
    {

        public static void AddRawMaterialInfo(Shape shape, Spec material, string size, ModelDoc2 modelDoc2)
        {
            if (shape == Shape.Beam || shape == Shape.Tee)
                material = Spec.A992;

            if ((shape == Shape.Plate) && size == "0.1344")
                material = Spec.A1011_33;


            string number;
            string description;
            if (JDE.TryGetValue((size, material), out var value))
            {
                number = value.RM;
                description = value.Description;
            }
            else
            {
                number = "N/A";
                description = $"{shape}_{size}_{material}";
            }
            SetProperty("RM", number, modelDoc2);
            SetProperty("RMDesc", description, modelDoc2);
            SetMaterial("Galvanized Steel", modelDoc2);

        }
        public static void SetMaterial(string material, ModelDoc2 modelDoc2)
        {
            PartDoc partDoc = modelDoc2 as PartDoc;

            MaterialVisualPropertiesData swMatVisPrps = partDoc.GetMaterialVisualProperties();
            swMatVisPrps.ApplyAppearance = false;
            partDoc.SetMaterialVisualProperties(swMatVisPrps, (int)swInConfigurationOpts_e.swAllConfiguration, "");

            string configName = modelDoc2.GetConfigurationNames()[0];
            object[] bodiesObj = partDoc.GetBodies2((int)swBodyType_e.swAllBodies, false);
            if (bodiesObj != null)
            {
                foreach (var bodyObj in bodiesObj)
                {
                    Body2 body = (Body2)bodyObj;
                    body.Select2(false, null);
                    int message = body.SetMaterialProperty(configName, "solidworks materials.sldmat", material);
                    modelDoc2.ClearSelection2(true);
                }
            }
        }







    }
}

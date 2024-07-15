using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace FileTools.CommonData.Headers.Connections
{
    public interface IConnection
    {
        string Location { get; set; }
        string ExtensionType { get; set; }

        double FlangeO { get; set; }
        double FlangeQ { get; set; }
        double FlangeR { get; set; }
        double FlangeX { get; set; }
        double FlangeRD { get; set; }
        double FlangeNB { get; set; }
        double FlangeDB { get; set; }
        double FlangeBC { get; set; }
        double FlangeYY { get; set; }

        double OD { get; set; }
        double Wall { get; set; }

        double Count { get; set; }
        double Spacing { get; set; }
        double OffsetX { get; set; }
        double ProjectionY { get; set; }
    }
}

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

        double O { get; set; }
        double Q { get; set; }
        double R { get; set; }
        double X { get; set; }
        double RD { get; set; }
        double NB { get; set; }
        double DB { get; set; }
        double BC { get; set; }
        double YY { get; set; }

        double OD { get; set; }
        double Wall { get; set; }

        double Count { get; set; }
        double Spacing { get; set; }
        double OffsetX { get; set; }
        double ExtensionY { get; set; }
    }
}

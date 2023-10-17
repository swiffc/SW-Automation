using System.Collections.Generic;

namespace Walkway
{
    internal class SteelBook
    {
        // Channel
        public static Dictionary<string, (double Depth, double WebTHK, double FlangeWidth, double AvgFlgTHK, double K, double FlangeGage, double WebGage)>
        Channel = new Dictionary<string, (double Depth, double WebTHK, double FlangeWidth, double AvgFlgTHK, double K, double FlangeGage, double WebGage)>
        {
            // Size             Depth      WebTHK      FlangeWidth     AvgFlgTHK   K           FlangeGage      WebGage
            { "C6x8.2",     (   6,         0.1875,     1.875,          0.3125,     0.8125,     1.125,          2.25)   },
            { "C8x11.5",    (   8,         0.25,       2.25,           0.375,      0.9375,     1.375,          2.5)    },
            { "C10x15.3",   (   10,        0.25,       2.625,          0.4375,     1,          1.5,            2.5)    }
        };

        // W-Shape
        public static Dictionary<string, (double Depth, double WebTHK, double FlangeWidth, double FlgTHK, double K, double FlangeGage, double WebGage)>
        W_Shape = new Dictionary<string, (double Depth, double WebTHK, double FlangeWidth, double FlgTHK, double K, double FlangeGage, double WebGage)>
        {
            // Size             Depth       WebTHK      FlangeWidth     FlgTHK      K           FlangeGage      WebGage
            { "W6x15",      (   6,          0.25,       6,              0.25,       0.625,      3.5,            2.25    )   },
            { "W6x20",      (   6.25,       0.25,       6,              0.375,      0.75,       3.5,            2.25    )   },
            { "W6x25",      (   6.375,      0.3125,     6.125,          0.4375,     0.8125,     3.5,            2.25    )   },
            { "W8x31",      (   8,          0.3125,     8,              0.4375,     0.9375,     5.5,            2.25    )   }
        };
    }
} 	  	  

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static FileTools.Properties.Settings;

namespace FileTools.Base
{
    public interface IHeaderExtensions
    {
        // Box
        double BoxWidth { get; set; }
        double TubesheetTHK { get; set; }
        double PlugsheetTHK { get; set; }

        // Bundle
        double TubeLength { get; set; }
        double TubeStickThru { get; set; }
        bool HeadersOutsideFrames { get; set; }
    }
}

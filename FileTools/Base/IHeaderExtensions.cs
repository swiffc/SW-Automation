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
        bool IsRequired { get; set; }

        double BoxWidth { get; set; }
        double VerticalSpan { get; set; }
        double BoxLength { get; set; }

        double TubesheetTHK { get; set; }
        double PlugsheetTHK { get; set; }
        double TopAndBottomPlateTHK { get; set; }
}
}

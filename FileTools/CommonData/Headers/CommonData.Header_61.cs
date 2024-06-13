using FileTools.Base;
using static FileTools.Properties.Settings;

namespace FileTools.CommonData
{
    public static partial class CommonData 
    {// 61 Header
        public static Header_61 Header61 = new Header_61();
        public class Header_61 : IHeaderExtensions
        {
            public double BoxWidth 
            { 
                get => Default.BoxWidth61; 
                set => Default.BoxWidth61 = value; 
            }
            public double TubesheetTHK 
            { 
                get => Default.TubesheetTHK_61; 
                set => Default.TubesheetTHK_61 = value; 
            }
            public double PlugsheetTHK
            {
                get => Default.PlugsheetTHK_61;
                set => Default.PlugsheetTHK_61 = value;
            }
            public bool IsRequired
            {
                get => Default.IsRequired_61;
                set => Default.IsRequired_61 = value;
            }
            public double VerticalSpan 
            {
                get => Default.VerticalSpan61; 
                set => Default.VerticalSpan61 = value;
            }
            public double BoxLength 
            { 
                get => Default.BoxLength61; 
                set => Default.BoxLength61 = value; 
            }
            public double TopAndBottomPlateTHK 
            { 
                get => Default.TopBottomTHK_61; 
                set => Default.TopBottomTHK_61 = value; 
            }
        }
    }
}

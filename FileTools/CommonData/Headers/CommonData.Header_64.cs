using FileTools.Base;
using static FileTools.Properties.Settings;

namespace FileTools.CommonData
{
    public static partial class CommonData
    {// 64 Header
        public static Header_64 Header64 = new Header_64();
        public class Header_64 : IHeaderExtensions
        {
            public double BoxWidth
            {
                get => Default.BoxWidth64;
                set => Default.BoxWidth64 = value;
            }
            public double TubesheetTHK
            {
                get => Default.TubesheetTHK_64;
                set => Default.TubesheetTHK_64 = value;
            }
            public double PlugsheetTHK
            {
                get => Default.PlugsheetTHK_64;
                set => Default.PlugsheetTHK_64 = value;
            }
            public bool IsRequired
            {
                get => Default.IsRequired_64;
                set => Default.IsRequired_64 = value;
            }
            public double VerticalSpan
            {
                get => Default.VerticalSpan64;
                set => Default.VerticalSpan64 = value;
            }
            public double BoxLength
            {
                get => Default.BoxLength64;
                set => Default.BoxLength64 = value;
            }
            public double TopAndBottomPlateTHK
            {
                get => Default.TopBottomTHK_64;
                set => Default.TopBottomTHK_64 = value;
            }
        }
    }
}

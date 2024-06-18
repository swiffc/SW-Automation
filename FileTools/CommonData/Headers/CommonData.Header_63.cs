using FileTools.Base;
using static FileTools.Properties.Settings;

namespace FileTools.CommonData
{
    public static partial class CommonData
    {// 63 Header
        public static Header_63 Header63 = new Header_63();
        public class Header_63 : IHeaderExtensions
        {
            public double BoxWidth
            {
                get => Default.BoxWidth63;
                set => Default.BoxWidth63 = value;
            }
            public double TubesheetTHK
            {
                get => Default.TubesheetTHK_63;
                set => Default.TubesheetTHK_63 = value;
            }
            public double PlugsheetTHK
            {
                get => Default.PlugsheetTHK_63;
                set => Default.PlugsheetTHK_63 = value;
            }
            public bool IsRequired
            {
                get => Default.IsRequired_63;
                set => Default.IsRequired_63 = value;
            }
            public double VerticalSpan
            {
                get => Default.VerticalSpan63;
                set => Default.VerticalSpan63 = value;
            }
            public double BoxLength
            {
                get => Default.BoxLength63;
                set => Default.BoxLength63 = value;
            }
            public double TopAndBottomPlateTHK
            {
                get => Default.TopBottomTHK_63;
                set => Default.TopBottomTHK_63 = value;
            }
            public double Y_Location
            {
                get => Default.Y_Location63;
                set => Default.Y_Location63 = value;
            }
        }
    }
}

using FileTools.Base;
using static FileTools.Properties.Settings;

namespace FileTools.CommonData
{
    public static partial class CommonData
    {// 62 Header
        public static Header_62 Header62 = new Header_62();
        public class Header_62 : IHeaderExtensions
        {
            public double BoxWidth
            {
                get => Default.BoxWidth62;
                set => Default.BoxWidth62 = value;
            }
            public double TubesheetTHK
            {
                get => Default.TubesheetTHK_62;
                set => Default.TubesheetTHK_62 = value;
            }
            public double PlugsheetTHK
            {
                get => Default.PlugsheetTHK_62;
                set => Default.PlugsheetTHK_62 = value;
            }
            public bool IsRequired
            {
                get => Default.IsRequired_62;
                set => Default.IsRequired_62 = value;
            }
            public double VerticalSpan
            {
                get => Default.VerticalSpan62;
                set => Default.VerticalSpan62 = value;
            }
            public double BoxLength
            {
                get => Default.BoxLength62;
                set => Default.BoxLength62 = value;
            }
            public double TopAndBottomPlateTHK
            {
                get => Default.TopBottomTHK_62;
                set => Default.TopBottomTHK_62 = value;
            }
        }
    }
}

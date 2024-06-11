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
            public double TubeLength
            {
                get => Default.TubeLength;
                set => Default.TubeLength = value;
            }
            public double TubeStickThru
            {
                get => Default.TubeStickThru;
                set => Default.TubeStickThru = value;
            }
            public bool HeadersOutsideFrames
            {
                get => Default.HeadersOutsideFrames;
                set => Default.HeadersOutsideFrames = value;
            }
        }
    }
}

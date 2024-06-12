using FileTools.Base;
using static FileTools.Properties.Settings;

namespace FileTools.CommonData
{
    public static partial class CommonData
    {// 65 Header
        public static Header_65 Header65 = new Header_65();
        public class Header_65 : IHeaderExtensions
        {
            public double BoxWidth
            {
                get => Default.BoxWidth65;
                set => Default.BoxWidth65 = value;
            }
            public double TubesheetTHK
            {
                get => Default.TubesheetTHK_65;
                set => Default.TubesheetTHK_65 = value;
            }
            public double PlugsheetTHK
            {
                get => Default.PlugsheetTHK_65;
                set => Default.PlugsheetTHK_65 = value;
            }
            public bool IsRequired
            {
                get => Default.IsRequired_65;
                set => Default.IsRequired_65 = value;
            }
        }
    }
}

using FileTools.Base;
using static FileTools.Properties.Settings;

namespace FileTools.CommonData
{
    public static partial class CommonData 
    {// 66 Header
        public static Header_66 Header66 = new Header_66();
        public class Header_66 : IHeaderExtensions
        {
            public double BoxWidth 
            { 
                get => Default.BoxWidth66; 
                set => Default.BoxWidth66 = value; 
            }
            public double TubesheetTHK 
            { 
                get => Default.TubesheetTHK_66; 
                set => Default.TubesheetTHK_66 = value; 
            }
            public double PlugsheetTHK
            {
                get => Default.PlugsheetTHK_66;
                set => Default.PlugsheetTHK_66 = value;
            }
            public bool IsRequired
            {
                get => Default.IsRequired_66;
                set => Default.IsRequired_66 = value;
            }
        }
    }
}

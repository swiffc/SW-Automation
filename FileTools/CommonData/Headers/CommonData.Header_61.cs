using FileTools.Base;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace FileTools.CommonData
{
    public static partial class CommonData
    {
        // 61 Header
        public static Header61Extensions Header61 { get; set; } = new Header61Extensions();
        public class Header61Extensions : IHeaderExtensions
        {
            public bool Enabled { get; set; } = true;
        }
    }
}

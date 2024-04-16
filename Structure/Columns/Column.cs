using FileTools.Base;
using ModelTools;
using Structure.Columns.Derived.Children;
using System;
using static FileTools.SharedProperties;

namespace Structure.Columns
{
    internal abstract class Column : SubAssembly
    {
        // Static properties
        static internal double Height => ColumnHeight - PlenumDepth - MachineryMountHeight + ShippingBeamHeight;


        // Constructor
        protected Column(SW_Assembly swAssembly) : base(swAssembly) { }
    }
}

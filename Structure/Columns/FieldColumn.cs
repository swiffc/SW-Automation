using FileTools.Base;
using ModelTools;
using Structure.Columns.Derived.Children;
using System;
using static FileTools.SharedProperties;

namespace Structure.Columns
{
    internal abstract class FieldColumn : SubAssembly
    {
        // Static properties
        static internal double Height => TotalColumnHeight - PlenumDepth - MachineryMountHeight + ShippingBeamHeight;


        // Constructor
        protected FieldColumn(SW_Assembly swAssembly) : base(swAssembly) { }
    }
}

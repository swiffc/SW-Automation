using FileTools.Base;
using ModelTools;
using System.Collections.Generic;
using static FileTools.CommonData.CommonData;

namespace Bundle.SideFrame.Derived.Children
{
    internal abstract class SideFramePart : Part
    {
        // Static properties
        public static double THK
        {
            get { return SideFrame_THK; }
            set { SideFrame_THK = value; }
        }


        // Constructor
        protected SideFramePart(SubAssembly parentSubAssembly) : base(parentSubAssembly) { }


        // Property overrides
        public override bool Enabled => true;
        public override Shape RawMaterialShape => Shape.Plate;
        public override string SizeOrThickness => THK.ToString();
    }
}

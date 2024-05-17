using FileTools.Base;
using ModelTools;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static FileTools.CommonData.CommonData;
using static FileTools.StaticFileTools;

namespace MachineryMount.DriveFrame.Children
{
    internal abstract class Stringer : Part
    {
        // Static properties
        public static string Size
        {
            get { return Stringer_Size; }
            set { Stringer_Size = value; }
        }
        public static double Depth => Stringer_Depth;


        // Constructor
        protected Stringer(SubAssembly parentSubAssembly) : base(parentSubAssembly) { }


        // Method overrides
        protected override void Dimensions()
        {
            ChangeStructuralMemberSize(Size, ModelDoc2);
            EditDimension("Length", "sk:Path", Plenum_Width);
        }


        // Property overrides
        public override bool Enabled => true;
        public override Shape RawMaterialShape => Shape.Channel;
        public override string SizeOrThickness => Size;
    }
}

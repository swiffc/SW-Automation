using FileTools.Base;
using ModelTools;
using System;
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
        public static double Depth
        {
            get { return SideFrame_Depth; }
            set { SideFrame_Depth = value; }
        }
        static public double Flange => 3;
        static public double FrontLength
        {
            get
            {
                if (HeadersOutsideFrames)
                {
                    double thickestTubesheet = Math.Max(Math.Max
                    (
                        Header61.TubesheetTHK,
                        Header63.TubesheetTHK),
                        Header65.TubesheetTHK
                    );
                    return TubeLength / 2 - TubeProjection - thickestTubesheet - LengthReduction;

                }
                else // Headers are inside the frame
                {
                    double widestHeader = Math.Max(Math.Max
                    (
                        Header61.BoxWidth + Header61.PlugsheetTHK,
                        Header63.BoxWidth + Header63.PlugsheetTHK),
                        Header65.BoxWidth + Header65.PlugsheetTHK
                    );

                    return TubeLength / 2 - TubeProjection + widestHeader + ExtraLength;
                }
            }
        }
        static public double RearLength
        {
            get
            {
                if (HeadersOutsideFrames)
                {
                    double thickestTubesheet = Math.Max(Math.Max
                    (
                        Header62.TubesheetTHK,
                        Header64.TubesheetTHK),
                        Header66.TubesheetTHK
                    );
                    return TubeLength / 2 - TubeProjection - thickestTubesheet - LengthReduction;

                }
                else // Headers are inside the frame
                {
                    double widestHeader = Math.Max(Math.Max
                    (
                        Header62.BoxWidth + Header61.PlugsheetTHK,
                        Header64.BoxWidth + Header63.PlugsheetTHK),
                        Header66.BoxWidth + Header65.PlugsheetTHK
                    );

                    return TubeLength / 2 - TubeProjection + widestHeader + ExtraLength;
                }
            }
        }
        static public double ExtraLength => 0.5;
        static public double LengthReduction => 0.375;


        // Constructor
        protected SideFramePart(SubAssembly parentSubAssembly) : base(parentSubAssembly) { }


        // Method overrides
        protected override void Dimensions()
        {
            EditDimension("THK", "SheetMetal", THK);
            EditDimension("R", "SheetMetal", BendTable.GetBendRadius(THK));

            EditDimension("Depth", "sk:Plate", Depth);
            EditDimension("Flange", "sk:Plate", Depth + (IsToedOut ? - Flange : + Flange));

            EditDimension("FrontLength", "Plate", FrontLength);
            EditDimension("RearLength", "Plate", RearLength);
        }


        // Property overrides
        public override bool Enabled => true;
        public override Shape RawMaterialShape => Shape.Plate;
        public override string SizeOrThickness => THK.ToString();
    }
}

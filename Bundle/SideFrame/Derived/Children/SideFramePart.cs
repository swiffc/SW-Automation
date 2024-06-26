using Bundle.AirSeals.Derived;
using Bundle.Misc;
using Bundle.TubeKeepers;
using Bundle.TubeKeepers.Children;
using Bundle.TubeSupports;
using Bundle.TubeSupports.Children;
using FileTools.Base;
using ModelTools;
using System;
using System.Collections.Generic;
using static FileTools.CommonData.CommonData;
using static ModelTools.BendTable;
using static Tools.ModelTools;

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
            EditDimension("R", "SheetMetal", GetBendRadius(THK));

            EditDimension("Depth", "sk:Plate", Depth);
            EditDimension("Flange", "sk:Plate", Depth + (IsToedOut ? -Flange : +Flange));

            EditDimension("FrontLength", "Plate", FrontLength);
            EditDimension("RearLength", "Plate", RearLength);

            AirSealHoles();
            KeeperHolesAndFeatureControl();
            SupportHolesAndFeatureControl();
            PlenumHolesAndFeatureControl();
        }


        // Private methods
        void AirSealHoles()
        {
            double y = THK + GetBendRadius(THK) + 1.15625;

            var _1013 = new Bottom_Front_AirSeal();
            EditDimension("y1013", "sk:WebHole", y);
            EditDimension("z1013", "sk:WebHole", _1013.Position[0].TranslationZ);

            var _1014 = new Bottom_Rear_AirSeal();
            EditDimension("y1014", "sk:WebHole", y);
            EditDimension("z1014", "sk:WebHole", _1014.Position[0].TranslationZ);

            var _1015 = new Top_Front_AirSeal();
            EditDimension("y1015", "sk:WebHole", y);
            EditDimension("z1015", "sk:WebHole", _1015.Position[0].TranslationZ);

            var _1016 = new Top_Rear_AirSeal();
            EditDimension("y1016", "sk:WebHole", y);
            EditDimension("z1016", "sk:WebHole", _1016.Position[0].TranslationZ);
        }
        void KeeperHolesAndFeatureControl()
        {
            // Base holes
            List<PositionData> pos;
            if (IsSmithco)
            {
                var keeper = new TubeKeeperBent();
                pos = keeper.Position;
                EditDimension("KeeperHeight", "sk:WebHole", TubeKeeperBent.BottomToSlotCenter);
                EditDimension("KeeperWidth", "sk:WebHole", 0.001);
            }
            else
            {
                var keeper = new TubeKeeperWeldment();
                pos = keeper.Position;
                EditDimension("KeeperHeight", "sk:WebHole", TubeKeeper_EndPlate.OffsetFromBottom);
                EditDimension("KeeperWidth", "sk:WebHole", TubeKeeper_EndPlate.SlotToSlot);
            }
            EditDimension("yKeeper", "sk:WebHole", pos[0].TranslationY);
            EditDimension("zKeeper", "sk:WebHole", pos[0].TranslationZ);

            // Patterned holes
            if (Cambered)
            {
                SuppressFeatures("KeeperHolesLinear");
                UnsuppressFeatures("KeeperHolesCambered");

                EditDimension("TubeLength", "sk:KeeperHole", Tube.Length);
                EditDimension("Camber", "sk:KeeperHole", Tube.Camber);
                EditDimension("yTube", "sk:KeeperHole", Header61.Y_Location - Header61.Xtop);
                EditDimension("zTube", "sk:KeeperHole", Tube.Length / 2);
                EditDimension("Angle", "sk:KeeperHole", 90 - Tube.GetSlopeAngleDegrees(Tube.SlopesPerFootList[0]));
                EditDimension("Count", "KeeperHolesCambered", TubeSupport.Quantity);
            }
            else
            {
                SuppressFeatures("KeeperHolesCambered");
                UnsuppressFeatures("KeeperHolesLinear");

                EditDimension("Spacing", "KeeperHolesLinear", TubeSupport.Spacing_Feet * 12);
                EditDimension("Count", "KeeperHolesLinear", TubeSupport.Quantity);
            }
        }
        void SupportHolesAndFeatureControl()
        {
            // Base holes
            EditDimension("ySupport", "sk:WebHole", TubeSupport.PositionDataList[0].TranslationY);
            EditDimension("SupportHoleOffset", "sk:WebHole", TubeSupport.TopOfSupportToFirstHole);
            EditDimension("SupportHoleWidth", "sk:WebHole", TubeSupport.MountingHoleWidth);
            EditDimension("SupportHoleHeight", "sk:WebHole", IsSmithco ? MountingAngle.HoleToHole : TubeSupport_EndPlate.HoleToHoleHeight);

            // Patterned holes
            if (Cambered)
            {
                SuppressFeatures("SupportHolesLinear");
                UnsuppressFeatures("SupportHolesCambered");

                EditDimension("Angle", "sk:SupportHole", 90 - Tube.GetSlopeAngleDegrees(Tube.SlopesPerFootList[Tube.RowCount - 1]));
            }
            else
            {
                SuppressFeatures("SupportHolesCambered");
                UnsuppressFeatures("SupportHolesLinear");

                EditDimension("Spacing", "SupportHolesLinear", TubeSupport.Spacing_Feet * 12);
                EditDimension("Count", "SupportHolesLinear", TubeSupport.Quantity);
            }
        }
        void PlenumHolesAndFeatureControl()
        {
            // Base Holes
            if (FrontLength > Plenum_Length / 2)
            {
                UnsuppressFeatures("PlenumHole", "PlenumHoles");

                EditDimension("HalfUnitLength", "sk:PlenumHole", Plenum_Length / 2);
                EditDimension("ColumnToPanelEdge", "sk:PlenumHole",
                    Plenum_Design == Design.Standard ? Beam_Depth / 2 + InterferenceClearance :
                    Plenum_Design == Design.Legacy ? Beam_FlangeGage / 2 - 1.25 : 0.001);

                HolePattern(SidePanelLength - 6, out double count, out double spacing);
                EditDimension("Count", "sk:PlenumHole", count);
                EditDimension("Spacing", "sk:PlenumHole", spacing);

                // Patterned holes
                if (Fan_Count > 1)
                {
                    UnsuppressFeatures("PlenumHolesPattern");

                    EditDimension("Spacing", "PlenumHolesPattern", Plenum_Length / Fan_Count);
                    EditDimension("Count", "PlenumHolesPattern", Fan_Count);
                }
                else SuppressFeatures("PlenumHolesPattern");
            }
            else SuppressFeatures("PlenumHole", "PlenumHoles");
        }


        // Property overrides
        public override bool Enabled => true;
        public override Shape RawMaterialShape => Shape.Plate;
        public override string SizeOrThickness => THK.ToString();
    }
}

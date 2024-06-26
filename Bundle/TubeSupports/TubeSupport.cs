using Bundle.Misc;
using Bundle.TubeSupports.Children;
using FileTools.Base;
using ModelTools;
using SolidWorks.Interop.sldworks;
using SolidWorks.Interop.swconst;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static FileTools.CommonData.CommonData;
using static FileTools.StaticFileTools;

namespace Bundle.TubeSupports
{
    internal class TubeSupport : SubAssembly
    {
        // Static properties
        static public int Priority => 1;
        public static List<PositionData> PositionDataList
        {
            get
            {
                var pos = new List<PositionData>();

                var yTranslations = Y_Translations();
                var zTranslation = Spacing_Feet * 12 * (Quantity - 1) / 2;

                for (int i = 0; i < yTranslations.Count; i++)
                {
                    pos.Add(PositionData.Create(tY: yTranslations[i], tZ: zTranslation - Spacing_Feet * 12 * i));
                }

                return pos;
            }
        }
        static public double TopOfSupportToFirstHole
        {
            get
            {
                if (IsSmithco)
                {
                    return FourHoles ? TubeSupportPart.Height / 2 - MountingAngle.HoleToHole / 2 : TubeSupportPart.Height / 2;
                }
                else
                {
                    return FourHoles ? 2.5 : 2;
                }
            }
        }
        static public bool FourHoles
        {
            get
            {
                if (IsSmithco)
                {
                    return TubeSupportPart.Height > 4 ? true : false;
                }
                else
                {
                    return TubeSupport_EndPlate.FourHoles;
                }
            }
        }
        static public double MountingHoleWidth
        {
            get
            {
                if (IsSmithco)
                {
                    return TubeSupportPart.THK + MountingAngle.Gage * 2;
                }
                else
                {
                    return TubeSupport_EndPlate.HoleToHoleWidth;
                }
            }
        }


        // Constructor
        public TubeSupport(SW_Assembly parentMainAssembly) : base(parentMainAssembly) { }


        // Method overrides
        protected override void Configurations()
        {
            Configuration configuration = ModelDoc2.GetConfigurationByName(StaticPartNo);

            if (IsSmithco)
                configuration.ChildComponentDisplayInBOM = (int)swChildComponentInBOMOption_e.swChildComponent_Promote;
            else
                configuration.ChildComponentDisplayInBOM = (int)swChildComponentInBOMOption_e.swChildComponent_Show;
        }


        // Public methods
        public static List<PositionData> CalculateKeeperPositionFrom(List<PositionData> tubeSupportPositionData)
        {
            var pos = new List<PositionData>();

            // Front tube end to first support
            double toFirstSupport_Feet = (Tube.Length / 12 - TubeSupportSpacing_Feet * (TubeSupportQuantity - 1)) / 2;

            for (int i = 0; i < tubeSupportPositionData.Count; i++)
            {
                double adjustment = VerticalTubeSpan(toFirstSupport_Feet + TubeSupportSpacing_Feet * i) + FinOD;

                double newY = tubeSupportPositionData[i].TranslationY + adjustment;

                PositionData positionData = PositionData.Create
                (
                    tY: newY,
                    tZ: tubeSupportPositionData[i].TranslationZ
                );
                pos.Add(positionData);
            }

            return pos;
        }
        public static double VerticalTubeSpan(double distanceFromFrontTubeEnd_Feet)
        {
            double verticalPitch = Tube.AllFrontVerticalPitches,
                upperTubeSlope, lowerTubeSlope, frontVerticalPitch, rearVerticalPitch, percentOfTubeLength;

            for (int i = 0; i < Tube.RowCount; i++)
            {
                upperTubeSlope = Tube.SlopesPerFootList[i];
                lowerTubeSlope = Tube.SlopesPerFootList[i + 1];
                frontVerticalPitch = Tube.FrontVerticalPitchesList[i];
                rearVerticalPitch = Tube.RearVerticalPitchesList[i];
                percentOfTubeLength = distanceFromFrontTubeEnd_Feet / (Tube.Length / 12);

                if (upperTubeSlope > lowerTubeSlope)
                {
                    // Gap is narrowing
                    double totalNarrowing = frontVerticalPitch - rearVerticalPitch;
                    verticalPitch -= totalNarrowing * percentOfTubeLength;
                }
                else if (upperTubeSlope < lowerTubeSlope)
                {
                    // Gap is widening
                    double totalWidening = rearVerticalPitch - frontVerticalPitch;
                    verticalPitch += totalWidening * percentOfTubeLength;
                }
            }

            return verticalPitch;
        }


        // Private methods
        static List<double> Y_Translations()
        {
            var yTranslations = new List<double>();

            // Find Y translation if no slope last pass
            double yTranslation = Header61.Y_Location - Header61.Xtop - Tube.AllFrontVerticalPitches - Tube.FinOD / 2;

            // Find the slope of last tube row
            double lastRowSlope = 0;
            double[] slopesPerFoot = new double[]
            {
                SlopePerFoot.Row1, SlopePerFoot.Row2, SlopePerFoot.Row3, SlopePerFoot.Row4,
                SlopePerFoot.Row5, SlopePerFoot.Row6, SlopePerFoot.Row7, SlopePerFoot.Row8, SlopePerFoot.Row9, SlopePerFoot.Row10
            };
            if (!slopesPerFoot.All(slope => slope == 0))
            {
                double[] verticalPitches = new double[]
                {
                    FrontVerticalPitch._1_2,
                    FrontVerticalPitch._2_3,
                    FrontVerticalPitch._3_4,
                    FrontVerticalPitch._4_5,
                    FrontVerticalPitch._5_6,
                    FrontVerticalPitch._6_7,
                    FrontVerticalPitch._7_8,
                    FrontVerticalPitch._8_9,
                    FrontVerticalPitch._9_10
                };
                for (int i = 0; i < verticalPitches.Length; i++)
                {
                    if (verticalPitches[i] == 0)
                    {
                        lastRowSlope = slopesPerFoot[i];
                        break;
                    }
                }

                if (lastRowSlope == 0)
                    lastRowSlope = slopesPerFoot[slopesPerFoot.Length - 1];
            }

            // Slope last pass & straight, uncambered bundle
            double spacing_Inches = Spacing_Feet * 12;
            double slopePerInch = lastRowSlope / 12;
            double supportedSpan = spacing_Inches * (Quantity - 1);
            double tubeEndToFirstSupport = (Tube.Length - supportedSpan) / 2;
            double firstRise = tubeEndToFirstSupport * slopePerInch;
            double rise = spacing_Inches * slopePerInch;

            yTranslations.Add(yTranslation - firstRise);
            for (int i = 1; i < Quantity; i++)
            {
                yTranslations.Add(yTranslations[i - 1] - rise);
            }

            if (Cambered)
            {
                yTranslations[0] += Tube.CamberAtLocation(tubeEndToFirstSupport);
                for (int i = 1; i < Quantity; i++)
                {
                    yTranslations[i] += Tube.CamberAtLocation(tubeEndToFirstSupport + spacing_Inches * i);
                }
            }

            return yTranslations;
        }


        // Property overrides
        public override bool Enabled => true;
        public override string StaticPartNo => "1560";

        public override List<PositionData> Position
        {
            get
            {
                if (_pos == null)
                {
                    _pos = PositionDataList;
                }
                return _pos;
            }
        }




        // Wrapper properties
        public static double Spacing_Feet
        {
            get => TubeSupportSpacing_Feet;
            set => TubeSupportSpacing_Feet = value;
        }
        public static double Quantity
        {
            get => TubeSupportQuantity;
            set => TubeSupportQuantity = value;
        }
    }
}

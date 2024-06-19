using Bundle.Misc;
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
        // Constructor
        public TubeSupport(SW_Assembly parentAssembly) : base(parentAssembly) { }


        // Method overrides
        protected override void Configurations()
        {
            Configuration configuration = ModelDoc2.GetConfigurationByName(StaticPartNo);

            if (IsSmithco)      
                configuration.ChildComponentDisplayInBOM = (int)swChildComponentInBOMOption_e.swChildComponent_Promote;
            else
                configuration.ChildComponentDisplayInBOM = (int)swChildComponentInBOMOption_e.swChildComponent_Show;
        }


        // Private methods
        static List<double> Y_Translations()
        {
            var yTranslations = new List<double>();

            // Find Y translation if no slope last pass
            double yTranslation =

                // center of top tube row at 61header
                Header61.Y_Location - Header61.Xtop

                // all vertical pitches
                - VerticalPitch._1_2
                - VerticalPitch._2_3
                - VerticalPitch._3_4
                - VerticalPitch._4_5
                - VerticalPitch._5_6
                - VerticalPitch._6_7
                - VerticalPitch._7_8
                - VerticalPitch._8_9
                - VerticalPitch._9_10

                // half of fin OD
                - Tube.FinOD / 2;

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
                    VerticalPitch._1_2,
                    VerticalPitch._2_3,
                    VerticalPitch._3_4,
                    VerticalPitch._4_5,
                    VerticalPitch._5_6,
                    VerticalPitch._6_7,
                    VerticalPitch._7_8,
                    VerticalPitch._8_9,
                    VerticalPitch._9_10
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
                var yTranslations = Y_Translations();
                var zTranslation = Spacing_Feet * 12 * (Quantity - 1) / 2;

                var pos = new List<PositionData>();

                for (int i = 0; i < yTranslations.Count; i++)
                {
                    pos.Add(PositionData.Create(tY: yTranslations[i], tZ: zTranslation - Spacing_Feet * 12 * i));
                }

                return pos;
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

using Bundle.TubeSupports;
using FileTools.Base;
using ModelTools;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static FileTools.CommonData.CommonData;

namespace Bundle.Misc
{
    internal class Pstrip : Part
    {
        // Static properties
        static public double Offset
        {
            get
            {
                double largestTubeRow = Math.Max(Tube_Row_1L, Tube_Row_2L);
                return (largestTubeRow - 1) / 2 * Tube.HorizPitch;
            }
        }


        // Constructor
        public Pstrip(SW_Assembly parentMainAssembly) : base(parentMainAssembly) { }


        // Method overrides
        protected override void Dimensions()
        {
            EditDimension("Hpitch", "sk:Strip", Tube.HorizPitch);
            EditDimension("Vpitch", "sk:Strip", Tube.FrontVerticalPitchesList[0]);
            EditDimension("FinOD", "sk:Strip", FinOD);
            EditDimension("Offset", "sk:Strip", Offset);
            EditDimension("Hpitch", "StripPattern", Tube.HorizPitch);
            EditDimension("Count", "StripPattern", Math.Max(Tube_Row_1L, Tube_Row_2L) - 1);
        }


        // Private methods
        List<double> Get_Z_TranslationsFrom(List<PositionData> tubeSupportPositionData)
        {
            var list = new List<double>();

            for (int i = 0; i < tubeSupportPositionData.Count; i++)
            {
                list.Add(tubeSupportPositionData[i].TranslationZ);
            }

            return list;
        }
        List<double> Y_Translations(double firstRowY, double distanceFromFrontTubeEnd_Feet)
        {
            var list = new List<double>();
            var vPitches = Tube.VerticalPitchListAt(distanceFromFrontTubeEnd_Feet);

            // Rows 1 & 2
            double value = firstRowY + (Cambered ? Tube.CamberAtLocation(distanceFromFrontTubeEnd_Feet * 12) : 0);
            list.Add(value);

            for (int i = 0; i < Tube.RowCount - 1; i++)
            {
                list.Add(value -= vPitches[i]);
            }

            return list;
        }


        // Property overrides
        public override bool Enabled => !IsSmithco;
        public override string StaticPartNo => "Pstrip";
        public override Shape RawMaterialShape => 0;
        public override string SizeOrThickness => "";
        public override List<PositionData> Position
        {
            get
            {
                if (_pos == null)
                {
                    _pos = new List<PositionData>();
                    double toFirstSupport_Feet = (Tube.Length / 12 - TubeSupportSpacing_Feet * (TubeSupportQuantity - 1)) / 2;

                    List<double> yTranslations;
                    List<double> zTranslations = Get_Z_TranslationsFrom(TubeSupport.PositionDataList);

                    for (int i = 0; i < zTranslations.Count; i++)
                    {
                        // For each tube support
                        double distanceFromFrontTubeEnd_Feet = toFirstSupport_Feet + TubeSupportSpacing_Feet * i;
                        double yRow1 = (Header61.Y_Location - Header61.Xtop) - (SlopePerFoot.Row1 * toFirstSupport_Feet) - (i * SlopePerFoot.Row1 * TubeSupport.Spacing_Feet);

                        for (int j = 0; j < Tube.RowCount; j++)
                        {
                            // Between each tube
                            yTranslations = Y_Translations(yRow1, distanceFromFrontTubeEnd_Feet);

                            double upperSlope = Tube.SlopesPerFootList[j];
                            double lowerSlope = Tube.SlopesPerFootList[j + 1];
                            double rotation = Tube.GetSlopeAngleDegrees(upperSlope);
                            double y = yTranslations[j];

                            // For every other row
                            if (j % 2 != 0)
                            {
                                rotation += 180;
                                y -= Tube.FrontVerticalPitchesList[j];
                            }

                            // For less tubes in first row
                            if (Tube_Row_2L > Tube_Row_1L)
                            {
                                rotation += 180;
                                if (j % 2 == 0) // even 
                                    y -= Tube.FrontVerticalPitchesList[j];
                                else // odd
                                    y += Tube.FrontVerticalPitchesList[j];
                            }

                            // Only add if the gap is not narrowing or widening
                            if (upperSlope == lowerSlope)
                                _pos.Add(PositionData.Create(tY: y, tZ: zTranslations[i], rX: rotation));
                        }
                    }

                    return _pos;
                }
                else return _pos;
            }
        }
    }
}

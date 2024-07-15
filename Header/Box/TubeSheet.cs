using FileTools.Base;
using ModelTools;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static HDR.HeaderBase;
using static FileTools.StaticFileTools;
using static FileTools.CommonData.CommonData;
using System.Diagnostics;
using System.IO;

namespace HDR.Box
{
    internal class TubeSheet : Part
    {
        // Constructor
        public TubeSheet(SW_Assembly parentMainAssembly) : base(parentMainAssembly) { }


        // Method overrides
        protected override void Dimensions()
        {
            // Plate
            EditDimension("Length", "sk:Plate", Length, 0);
            EditDimension("Width", "sk:Plate", Width, 0);
            EditDimension("TopTHK", "sk:Plate", TopBtmPlate.THK, 0);
            EditDimension("EndTHK", "sk:Plate", EndPlate.THK, 0);
            EditDimension("THK", "Plate", THK, 0);

            // Hole
            EditDimension("Diameter", "sk:TubeHole", Header.TubeHoleDiameter, 0);
            EditDimension("OddY", "sk:TubeHole", Header.TubeY, 0);
            EditDimension("OddX", "sk:TubeHole", Header.TubeOddX, 0);
            EditDimension("EvenX", "sk:TubeHole", Header.TubeEvenX, 0);

            // Row1
            EditDimension("OddCount1", "sk:TubeHole", Header.TubeRow1Count, 2);
            EditDimension("OddSpacing1", "sk:TubeHole", Header.TubeHPitchOdd, 0);
            EditDimension("VerticalPitch_1_2", "sk:TubeHole", Header.TubeVPitchOneTwo
                != 0 ? Header.TubeVPitchOneTwo : Width, 0);

            // Row2
            EditDimension("EvenCount2", "sk:TubeHole", Header.TubeRow2Count, 2);
            EditDimension("EvenSpacing2", "sk:TubeHole", Header.TubeHPitchEven, 0);
            EditDimension("VerticalPitch_2_3", "sk:TubeHole", Header.TubeVPitchOneTwo
                != 0 && Header.TubeVPitchTwoThree != 0 ? Header.TubeVPitchTwoThree : Width, 0);

            // Row3
            EditDimension("OddCount3", "sk:TubeHole", Header.TubeRow3Count, 2);
            EditDimension("OddSpacing3", "sk:TubeHole", Header.TubeHPitchOdd, 0);
            EditDimension("VerticalPitch_3_4", "sk:TubeHole", Header.TubeVPitchTwoThree
                != 0 && Header.TubeVPitchThreeFour != 0 ? Header.TubeVPitchThreeFour : Width, 0);

            // Row4
            EditDimension("EvenCount4", "sk:TubeHole", Header.TubeRow4Count, 2);
            EditDimension("EvenSpacing4", "sk:TubeHole", Header.TubeHPitchEven, 0);
            EditDimension("VerticalPitch_4_5", "sk:TubeHole", Header.TubeVPitchThreeFour
                != 0 && Header.TubeVPitchFourFive != 0 ? Header.TubeVPitchFourFive : Width, 0);

            // Row5
            EditDimension("OddCount5", "sk:TubeHole", Header.TubeRow5Count, 2);
            EditDimension("OddSpacing5", "sk:TubeHole", Header.TubeHPitchOdd, 0);
            EditDimension("VerticalPitch_5_6", "sk:TubeHole", Header.TubeVPitchFourFive
                != 0 && Header.TubeVPitchFiveSix != 0 ? Header.TubeVPitchFiveSix : Width, 0);

            // Row6
            EditDimension("EvenCount6", "sk:TubeHole", Header.TubeRow6Count, 2);
            EditDimension("EvenSpacing6", "sk:TubeHole", Header.TubeHPitchEven, 0);
            EditDimension("VerticalPitch_6_7", "sk:TubeHole", Header.TubeVPitchFiveSix
                != 0 && Header.TubeVPitchSixSeven != 0 ? Header.TubeVPitchSixSeven : Width, 0);

            // Row7                                                                              
            EditDimension("OddCount7", "sk:TubeHole", Header.TubeRow7Count, 2);
            EditDimension("OddSpacing7", "sk:TubeHole", Header.TubeHPitchOdd, 0);
            EditDimension("VerticalPitch_7_8", "sk:TubeHole", Header.TubeVPitchSixSeven
                != 0 && Header.TubeVPitchSevenEight != 0 ? Header.TubeVPitchSevenEight : Width, 0);

            // Row8                                                                              
            EditDimension("EvenCount8", "sk:TubeHole", Header.TubeRow8Count, 2);
            EditDimension("EvenSpacing8", "sk:TubeHole", Header.TubeHPitchEven, 0);
            EditDimension("VerticalPitch_8_9", "sk:TubeHole", Header.TubeVPitchSevenEight
                != 0 && Header.TubeVPitchEightNine != 0 ? Header.TubeVPitchEightNine : Width, 0);

            // Row9                                                                             
            EditDimension("OddCount9", "sk:TubeHole", Header.TubeRow9Count, 2);
            EditDimension("OddSpacing9", "sk:TubeHole", Header.TubeHPitchOdd, 0);
            EditDimension("VerticalPitch_9_10", "sk:TubeHole", Header.TubeVPitchEightNine
                != 0 && Header.TubeVPitchNineTen != 0 ? Header.TubeVPitchNineTen : Width, 0);

            // Row10                                                                             
            EditDimension("EvenCount10", "sk:TubeHole", Header.TubeRow10Count, 2);
            EditDimension("EvenSpacing10", "sk:TubeHole", Header.TubeHPitchEven, 0);
            EditDimension("VerticalPitch_10_11", "sk:TubeHole", Header.TubeVPitchNineTen
                != 0 && Header.TubeVPitchTenEleven != 0 ? Header.TubeVPitchTenEleven : Width, 0);

            // Row11                                                                             
            EditDimension("OddCount11", "sk:TubeHole", Header.TubeRow1Count, 2);
            EditDimension("OddSpacing11", "sk:TubeHole", Header.TubeHPitchOdd, 0);
            EditDimension("VerticalPitch_11_12", "sk:TubeHole", Header.TubeVPitchTenEleven
                != 0 && Header.TubeVPitchElevenTwelve != 0 ? Header.TubeVPitchElevenTwelve : Width, 0);

            // Row12
            EditDimension("EvenCount12", "sk:TubeHole", Header.TubeRow2Count, 2);
            EditDimension("EvenSpacing12", "sk:TubeHole", Header.TubeHPitchEven, 0);
        }


        // Property overrides
        public override string PartNo => Header.TubesheetPartNo;
        public override bool Enabled => true;
        public override string StaticPartNo => "TubeSheet";
        public override Shape RawMaterialShape => Shape.Plate;
        public override string SizeOrThickness => THK.ToString();
        public override List<PositionData> Position
        {
            get
            {
                if (_pos == null)
                {
                    double xTranslation = 0;
                    double yTranslation = 0;
                    double zTranslation = -Header.BoxWidth / 2;

                    return new List<PositionData>
                    {
                        PositionData.Create(tX: xTranslation, tY: yTranslation, tZ: zTranslation),
                    };
                }
                return _pos;
            }
        }


        // Wrapper properties
        static public double THK
        {
            get => Header.TubesheetTHK;
        }
        static public double Length
        {
            get => Header.TubesheetLength - ModelLengthReduction;
        }
        static public double Width
        {
            get => Header.TubesheetWidth;
        }


        // Lists
        internal static List<double> HoleLocations = new List<double>
        {
            Header.TubeY,
            Header.TubeVPitchOneTwo,
            Header.TubeVPitchTwoThree,
            Header.TubeVPitchThreeFour,
            Header.TubeVPitchFourFive,
            Header.TubeVPitchFiveSix,
            Header.TubeVPitchSixSeven,
            Header.TubeVPitchSevenEight,
            Header.TubeVPitchEightNine,
            Header.TubeVPitchNineTen,
            Header.TubeVPitchTenEleven,
            Header.TubeVPitchElevenTwelve
        };
    }
}

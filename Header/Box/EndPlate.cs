using FileTools.Base;
using ModelTools;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static HDR.HeaderBase;
using static Excel.Prego;
using static FileTools.CommonData.CommonData;

namespace HDR.Box
{
    internal class EndPlate : Part
    {
        // Constructor
        public EndPlate(SW_Assembly parentMainAssembly) : base(parentMainAssembly) { }


        // Method overrides
        protected override void Dimensions()
        {
            EditDimension("Length", "sk:Plate", Length, 0);
            EditDimension("Width", "sk:Plate", Width, 0);
            EditDimension("THK", "Plate", THK, 0);
        }


        // Property overrides
        public override string PartNo => "AD";
        public override bool Enabled => true;
        public override Shape RawMaterialShape => Shape.Plate;
        public override string SizeOrThickness => THK.ToString();
        public override string StaticPartNo => "EndPlate";
        public override List<PositionData> Position
        {
            get
            {
                if (_pos == null)
                {
                    double xTranslation = TubeSheet.Length / 2;
                    double yTranslation;
                    if (Header.IsBusted)
                    {
                        yTranslation = -BustSpans[0] / 2;
                    }
                    else
                    {
                        yTranslation = -Header.BoxHeight / 2;
                    }
                    double zTranslation = 0;

                    return new List<PositionData>
                    {
                        PositionData.Create(tX: xTranslation, tY: yTranslation, tZ: zTranslation),
                        PositionData.Create(tX: -xTranslation, tY: yTranslation, tZ: zTranslation, rY: 180),
                    };
                }
                return _pos;
            }
        }


        // Bust data
        private List<double> _bustSpans;
        protected List<double> BustSpans
        {
            get
            {
                if (_bustSpans == null)
                {
                    string[] bustedSpans;
                    switch (Header)
                    {
                        case Header_61 Header61:
                            bustedSpans = CellNameColumnArray("AAF5", "AAF43");
                            break;
                        case Header_62 Header62:
                            bustedSpans = CellNameColumnArray("ACZ5", "ACZ43");
                            break;
                        case Header_63 Header63:
                            bustedSpans = CellNameColumnArray("AAJ5", "AAJ43");
                            break;
                        case Header_64 Header64:
                            bustedSpans = CellNameColumnArray("ADA5", "ADA43");
                            break;
                        case Header_65 Header65:
                            bustedSpans = CellNameColumnArray("AAK5", "AAK43");
                            break;
                        case Header_66 Header66:
                            bustedSpans = CellNameColumnArray("ADB5", "ADB43");
                            break;
                        default: throw new NotImplementedException();
                    }
                    _bustSpans = CellDoubleList(InputsCalcsSheet, bustedSpans);
                }
                return _bustSpans;
            }
        }


        // Virtual properties
        public virtual double Length
        {
            get
            {
                if (!_length.HasValue)
                {
                    if (Header.IsBusted)
                    {
                        if (BustSpans.Count >= 1)
                        {
                            var bustedLength = BustSpans[0];
                            _length = bustedLength;
                        }
                    }
                    else
                    {
                        _length = Header.EndPlateLength;
                    }
                }
                return _length.Value;
            }
        }


        // Wrapper properties
        static public double THK
        {
            get => Header.EndPlateTHK;
        }
        private double? _length;
        static public double Width
        {
            get => Header.EndPlateWidth > Header.BoxWidth ? Header.BoxWidth : Header.EndPlateWidth;
        }
        static public bool IsBusted => Header.IsBusted;
    }
}

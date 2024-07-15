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
            EditDimension("Length", "sk:Plate", LocalLength, 0);
            EditDimension("Width", "sk:Plate", Width, 0);
            EditDimension("THK", "Plate", THK, 0);
        }


        // Property overrides
        public override string PartNo => Header.EndPlatePartNo;
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
                        yTranslation = -Length1 / 2;
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


        // Virtual properties
        protected virtual double LocalLength => Length1;


        // Wrapper properties
        public double Length1
        {
            get
            {
                if (!_length.HasValue)
                {
                    if (Header.IsBusted)
                    {
                        _length = Header.EndPlateLength + 0.25;
                    }
                    else
                    {
                        _length = Header.BoxHeight;
                    }
                }
                return _length.Value;
            }
        }
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

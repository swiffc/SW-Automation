using FileTools.Base;
using FileTools.CommonData.Headers.Connections;
using HDR.Box;
using ModelTools;
using System;
using System.Collections.Generic;
using static FileTools.CommonData.CommonData;
using static HDR.HeaderBase;

namespace HDR.Connections
{
    internal abstract class Extension : Part
    {
        // Static properties
        static public double WeldGap => IsSmithco ? 0.09375 : 0.0625;


        // Constructor
        protected Extension(SW_Assembly parentMainAssembly) : base(parentMainAssembly) { }


        // Property overrides
        public override string SizeOrThickness => "";
        public override List<PositionData> Position
        {
            get
            {
                if (_pos == null)
                {
                    bool bottom = Location.StartsWith("B");

                    double xTranslation = OffsetX;
                    double yTranslation = (TopBtmPlate.THK + Length / 2 + WeldGap) * (bottom ? -1 : 1) - (bottom ? Header.BoxHeight : 0);
                    double zTranslation = 0;
                    double xRotation = bottom ? 180 : 0;

                    _pos = new List<PositionData>
                    {
                        PositionData.Create(tX: xTranslation, tY: yTranslation, tZ: zTranslation, rX: xRotation),
                    };

                    for (int i = 1; i < Count; i++)
                    {
                        xTranslation -= Spacing;
                        _pos.Add(PositionData.Create(tX: xTranslation, tY: yTranslation, tZ: zTranslation, rX: xRotation));
                    }
                }
                return _pos;
            }
        }


        // Protected properties
        protected double Length
        {
            get
            {
                double length = TopBtmPlate.THK + Inlet.FlangeYY + Inlet.FlangeRD + WeldGap * 2;
                double flangeYLocation = Flange.CalculateYTranslation(Ext.ProjectionY, Ext.Location);

                if (flangeYLocation < 0)
                {
                    return Math.Abs(flangeYLocation + Header.BoxHeight + length);
                }
                else
                {
                    return flangeYLocation - length;
                }
            }
        }


        // Abstract properties
        protected abstract IConnection Ext { get; }


        // Wrapper properties
        protected string Location => Ext.Location;
        protected double OffsetX => Ext.OffsetX;
        protected double Count => Ext.Count;
        protected double Spacing => Ext.Spacing;
    }
}

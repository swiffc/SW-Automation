using FileTools.Base;
using FileTools.CommonData.Headers.Connections;
using HDR.Box;
using HDR.Connections.Derived.Derived;
using Microsoft.Office.Interop.Excel;
using ModelTools;
using System;
using System.Collections.Generic;
using static FileTools.CommonData.CommonData;
using static HDR.HeaderBase;
using static FileTools.Base.SW_Assembly;

namespace HDR.Connections
{
    internal abstract class Extension : Part
    {
        // Static properties
        static public double WeldGap => IsSmithco ? 0.09375 : 0.0625;


        // Constructor
        protected Extension(SW_Assembly parentMainAssembly) : base(parentMainAssembly)
        {
            var loadPositionData = Position;

            if (IdenticalExtensions)
                DontProcessLocation.Add(typeof(OutletNozzle));
        }


        // Private properties
        bool IdenticalExtensions => Inlet.ExtensionPartNo == Outlet.ExtensionPartNo;


        // Property overrides
        public override string SizeOrThickness => "";
        public override List<PositionData> Position
        {
            get
            {
                if (_staticPos == null) _staticPos = new List<PositionData>();

                if (_posInlet == null && Ext is InletNozzle && Enabled)
                {
                    _posInlet = NewPositionData();
                    _staticPos.AddRange(_posInlet);
                }
                else if (_posOutlet == null && Ext is OutletNozzle && Enabled)
                {
                    _posOutlet = NewPositionData();
                    _staticPos.AddRange(_posOutlet);
                }

                if (IdenticalExtensions)
                    return _staticPos;
                else if (Ext is InletNozzle)
                    return _posInlet;
                else if (Ext is OutletNozzle)
                    return _posOutlet;
                else throw new Exception("Extension is neither Inlet nor Outlet.");
            }
        }


        // Public methods
        public static void ClearPositionData()
        {
            _staticPos = null;
            _posInlet = null;
            _posOutlet = null;
        }


        // Private methods
        List<PositionData> NewPositionData()
        {

            bool bottom = Location.StartsWith("B");

            double xTranslation = OffsetX;
            double yTranslation = (TopBtmPlate.THK + Length / 2 + WeldGap) * (bottom ? -1 : 1) - (bottom ? Header.BoxHeight : 0);
            double zTranslation = 0;
            double xRotation = bottom ? 180 : 0;

            var pos = new List<PositionData>
            {
                PositionData.Create(tX: xTranslation, tY: yTranslation, tZ: zTranslation, rX: xRotation),
            };

            for (int i = 1; i < Count; i++)
            {
                xTranslation -= Spacing;
                pos.Add(PositionData.Create(tX: xTranslation, tY: yTranslation, tZ: zTranslation, rX: xRotation));
            }

            return pos;
        }


        // Protected properties
        protected double Length
        {
            get
            {
                double length = TopBtmPlate.THK + Ext.FlangeYY + Ext.FlangeRD + WeldGap * 2;
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


        // Backing fields
        static List<PositionData> _staticPos;
        static List<PositionData> _posInlet;
        static List<PositionData> _posOutlet;
    }
}

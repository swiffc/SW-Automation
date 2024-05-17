using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static FileTools.Properties.Settings;

namespace FileTools.CommonData
{
    public partial class CommonData
    {
        // Job
        static public string Project
        {
            get { return Default.Project; }
            set { Default.Project = value; }
        }
        static public char Bank
        {
            get { return Default.Bank; }
            set { Default.Bank = value; }
        }
        static public string Customer
        {
            get { return Default.Customer; }
            set { Default.Customer = value; }
        }
        static public string Client
        {
            get { return Default.Client; }
            set { Default.Client = value; }

        }
        static public string PlantLocation
        {
            get { return Default.PlantLocation; }
            set { Default.PlantLocation = value; }
        }
        static public string PurchaseOrder
        {
            get { return Default.PurchaseOrder; }
            set { Default.PurchaseOrder = value; }
        }
        static public string ItemNumber
        {
            get { return Default.ItemNumber; }
            set { Default.ItemNumber = value; }
        }
        static public string Initials
        {
            get { return Default.Initials; }
            set { Default.Initials = value; }
        }

        // Misc
        static public string MaterialSpecSetting
        {
            get { return Default.MaterialSpecSetting; }
            set { Default.MaterialSpecSetting = value; }
        }
        static public int TotalUnitWeight
        {
            get { return Default.TotalUnitWeight; }
            set { Default.TotalUnitWeight = value; }
        }


        // Performance
        static public bool Toggle_CreateDrawing
        {
            get { return Default.Toggle_CreateDrawing; }
            set { Default.Toggle_CreateDrawing = value; }
        }
        static public bool Toggle_Save
        {
            get { return Default.Toggle_Save; }
            set { Default.Toggle_Save = value; }
        }
        static public bool Toggle_DeleteFiles
        {
            get { return Default.Toggle_DeleteFiles; }
            set { Default.Toggle_DeleteFiles = value; }
        }
    }
}

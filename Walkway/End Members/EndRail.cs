using SolidWorks.Interop.sldworks;

namespace Walkway
{
    internal class EndRail : Walkway
    {
        // Contructor
        public EndRail()
        {
            ModifyEndRail();
        }

        // extensions
        public EndRailExtensions Rail { get; set; } = new EndRailExtensions();
        public class EndRailExtensions
        {
            public int    SeriesNumber { get; set; } = 1;
            public string FilePath => $@"needs fixed";
            public double Leg { get; set; } = 2.5;
            public string Title { get; set; } = "END-RAIL_WW";
            public string Description { get; set; } = "ANGLE_2-1/2\"x2-1/2\"x1/4\"_A572-50";
            public string JDEnumber { get; set; } = "53499";
            public string PartNo => $"{SeriesNumber}351";
        }
        
        private void ModifyEndRail()
        {

            // Open SOLIDWORKS file and obtain COM reference
            ModelDoc2 rail = Open(Rail.FilePath);

            EditDimension("length", "body", 40.0000000000000000000000 + Rail.Leg * 2, rail);

            // Job info
            SetProperty("Project", Project, rail);
            SetProperty("Bank", Bank, rail);
            SetProperty("Customer", Customer, rail);
            SetProperty("Client", Client, rail);
            SetProperty("Location", Location, rail);
            SetProperty("PO", PurchaseOrder, rail);
            SetProperty("ItemNo", ItemNumber, rail);

            // JDE info
            SetProperty("RMdesc", Rail.Description, rail);
            SetProperty("PartNo", Rail.PartNo, rail);
            SetProperty("RM", Rail.JDEnumber, rail);
            SetProperty("Title", Rail.Title, rail);

        }

    }
}

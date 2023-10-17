using SolidWorks.Interop.sldworks;
using Walkway.Tools;

namespace Walkway
{
    internal class EndToePlate : Walkway
    {
        // constructor
        public EndToePlate()
        {
            PassToePlateValues();
        }

        //extensions
        public EndToePlateExtensions ToePlate { get; set; } = new EndToePlateExtensions();
        public class EndToePlateExtensions
        {
            public int    SeriesNumber { get; set; } = 1;
            public string FilePath => $@"needs fixed";
            public double Height { get; set; } = 5;
            public string Title { get; set; } = "END-TOE-PLATE_WW";
            public string Description { get; set; } = "PLATE_1/4\"_A572_50";
            public string JDEnumber { get; set; } = "60038";
            public string PartNo => $"{SeriesNumber}354";
        }

        private void PassToePlateValues()
        {
          

            // Open SOLIDWORKS file and obtain COM reference
            ModelDoc2 toePlate = Open(ToePlate.FilePath);

            // Job info
            SetProperty("Project", Project, toePlate);
            SetProperty("Bank", Bank, toePlate);
            SetProperty("Customer", Customer, toePlate);
            SetProperty("Client", Client, toePlate);
            SetProperty("Location", Location, toePlate);
            SetProperty("PO", PurchaseOrder, toePlate);
            SetProperty("ItemNo", ItemNumber, toePlate);

            // JDE info
            SetProperty("RMdesc", ToePlate.Description, toePlate);
            SetProperty("PartNo", ToePlate.PartNo, toePlate);
            SetProperty("RM", ToePlate.JDEnumber, toePlate);
            SetProperty("Title", ToePlate.Title, toePlate);

            EditDimension("Width", "profile", 40.000000000000000, toePlate);
            switch ('L')
            {
                case 'L':
                    EditDimension("hole1", "holes", ToePlate.Height / 2, toePlate);
                    break;
                case 'C':
                    EditDimension("hole1", "holes", ToePlate.Height / 2 - 1.5, toePlate);
                    break;
            }

            // Release COM object, but leave file open in SOLIDWORKS for assembly placement
            Optimize.Release(ref toePlate);

           
        }
    }
}

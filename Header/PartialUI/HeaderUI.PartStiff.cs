using System;
using System.Collections.Generic;
using System.Windows.Forms;
using static Excel.Prego;
using static FileTools.CommonData.CommonData;

namespace HDR
{
    public partial class HeaderUI
    {
        // Primary method
        private void ImportPartStiffPartNumbers()
        {
            ImportPartStiffPartNumbers_135();
            ImportPartStiffPartNumbers_246();

            SaveSettings();
        }

        // Primary sub-methods
        private void ImportPartStiffPartNumbers_135()
        {
            // Mapped data to be processed
            List<double> pTHK_Process_135 = InitializePTHKProcess_135();
            List<IPropertyWrapper> pApp_Process_135 = InitializePAppProcess_135();
            List<TextBox> pUI_Process_135 = InitializePUIProcess_135();
            List<double> sTHK_Process_135 = InitializeSTHKProcess_135();
            List<IPropertyWrapper> sApp_Process_135 = InitializeSAppProcess_135();
            List<TextBox> sUI_Process_135 = InitializeSUIProcess_135();

            // Get order and quantity of partitions and stiffeners for all odd numbered headers
            List<string> partStiff_135 = InitializePartStiffList_135();

            // Lists to hold observed values
            var pTHK_Observed_135 = new List<double>();
            var pApp_Observed_135 = new List<IPropertyWrapper>();
            var pUI_Observed_135 = new List<TextBox>();

            var sTHK_Observed_135 = new List<double>();
            var sApp_Observed_135 = new List<IPropertyWrapper>();
            var sUI_Observed_135 = new List<TextBox>();

            // Flags to clear observed values
            bool pClear = false;
            bool sClear = false;

            // Prego part number cells
            var partNumberCells_1 = InitializePartNumberCells_1();
            var partNumberCells_3 = InitializePartNumberCells_3();
            var partNumberCells_5 = InitializePartNumberCells_5();

            // Clear existing partNo's
            ClearPartStiffData(pApp_Process_135, pUI_Process_135, sApp_Process_135, sUI_Process_135);

            // Main loop to assign part numbers
            ProcessPartStiffLoop(
                partStiff_135, pTHK_Process_135, pApp_Process_135, pUI_Process_135,
                pTHK_Observed_135, pApp_Observed_135, pUI_Observed_135,
                ref pClear,
                sTHK_Process_135, sApp_Process_135, sUI_Process_135,
                sTHK_Observed_135, sApp_Observed_135, sUI_Observed_135,
                ref sClear,
                partNumberCells_1, partNumberCells_3, partNumberCells_5);
        }
        private void ImportPartStiffPartNumbers_246()
        {
            List<double> pTHK_Process_246 = InitializePTHKProcess_246();
            List<IPropertyWrapper> pApp_Process_246 = InitializePAppProcess_246();
            List<TextBox> pUI_Process_246 = InitializePUIProcess_246();
            List<double> sTHK_Process_246 = InitializeSTHKProcess_246();
            List<IPropertyWrapper> sApp_Process_246 = InitializeSAppProcess_246();
            List<TextBox> sUI_Process_246 = InitializeSUIProcess_246();

            List<string> partStiff_246 = InitializePartStiffList_246();

            var pTHK_Observed_246 = new List<double>();
            var pApp_Observed_246 = new List<IPropertyWrapper>();
            var pUI_Observed_246 = new List<TextBox>();

            var sTHK_Observed_246 = new List<double>();
            var sApp_Observed_246 = new List<IPropertyWrapper>();
            var sUI_Observed_246 = new List<TextBox>();

            bool pClear = false;
            bool sClear = false;

            var partNumberCells_2 = InitializePartNumberCells_2();
            var partNumberCells_4 = InitializePartNumberCells_4();
            var partNumberCells_6 = InitializePartNumberCells_6();

            ClearPartStiffData(pApp_Process_246, pUI_Process_246, sApp_Process_246, sUI_Process_246);

            ProcessPartStiffLoop(
                partStiff_246, pTHK_Process_246, pApp_Process_246, pUI_Process_246,
                pTHK_Observed_246, pApp_Observed_246, pUI_Observed_246,
                ref pClear,
                sTHK_Process_246, sApp_Process_246, sUI_Process_246,
                sTHK_Observed_246, sApp_Observed_246, sUI_Observed_246,
                ref sClear,
                partNumberCells_2, partNumberCells_4, partNumberCells_6);
        }


        // Sub-methods
        private List<string> InitializePartStiffList_246()
        {
            var list = new List<string>
            {
                CellString(InputSheet, "AL6", "AM6"),
                CellString(InputSheet, "AL8", "AM8"),
                CellString(InputSheet, "AL10", "AM10"),
                CellString(InputSheet, "AL12", "AM12"),
                CellString(InputSheet, "AL14", "AM14"),
                CellString(InputSheet, "AL16", "AM16"),
                CellString(InputSheet, "AL18", "AM18"),
                CellString(InputSheet, "AL20", "AM20"),
                CellString(InputSheet, "AL22", "AM22"),
                CellString(InputSheet, "AL24", "AM24"),
                CellString(InputSheet, "AL26", "AM26"),
                CellString(InputSheet, "AL28", "AM28")
            };

            for (int i = 0; i < list.Count; i++)
            {
                if (string.IsNullOrEmpty(list[i]) || list[i] == " Bot")
                {
                    list.RemoveAt(i);
                    i--;
                }
            }

            return list;
        }

        private List<string> InitializePartStiffList_135()
        {
            var list = new List<string>
            {
                CellString(InputSheet, "AB6", "AC6"),
                CellString(InputSheet, "AB8", "AC8"),
                CellString(InputSheet, "AB10", "AC10"),
                CellString(InputSheet, "AB12", "AC12"),
                CellString(InputSheet, "AB14", "AC14"),
                CellString(InputSheet, "AB16", "AC16"),
                CellString(InputSheet, "AB18", "AC18"),
                CellString(InputSheet, "AB20", "AC20"),
                CellString(InputSheet, "AB22", "AC22"),
                CellString(InputSheet, "AB24", "AC24"),
                CellString(InputSheet, "AB26", "AC26"),
                CellString(InputSheet, "AB28", "AC28")
            };

            for (int i = 0; i < list.Count; i++)
            {
                if (string.IsNullOrEmpty(list[i]) || list[i] == " Bot")
                {
                    list.RemoveAt(i);
                    i--;
                }

            }

            return list;
        }
        private void ClearPartStiffData(List<IPropertyWrapper> pAppProcess, List<TextBox> pUIProcess, List<IPropertyWrapper> sAppProcess, List<TextBox> sUIProcess)
        {
            for (int i = 0; i < pAppProcess.Count; i++)
                pAppProcess[i].SetValue(null);
            for (int i = 0; i < sAppProcess.Count; i++)
                sAppProcess[i].SetValue(null);

            for (int i = 0; i < pUIProcess.Count; i++)
                pUIProcess[i].Text = "";
            for (int i = 0; i < sUIProcess.Count; i++)
                sUIProcess[i].Text = "";
        }
        private void ProcessPartStiffLoop(List<string> partStiff, List<double> pTHK_Process, List<IPropertyWrapper> pApp_Process, List<TextBox> pUI_Process, List<double> pTHK_Observed, List<IPropertyWrapper> pApp_Observed, List<TextBox> pUI_Observed, ref bool pClear, List<double> sTHK_Process, List<IPropertyWrapper> sApp_Process, List<TextBox> sUI_Process, List<double> sTHK_Observed, List<IPropertyWrapper> sApp_Observed, List<TextBox> sUI_Observed, ref bool sClear, List<string> partNumberCells_1, List<string> partNumberCells_3, List<string> partNumberCells_5)
        {
            // Flags to prevent multiple assignments
            bool pFlag = false;
            bool sFlag = false;

            // Process first as partition
            if (partStiff[0].Contains("P") && pTHK_Observed.Count == 0)
            {
                ProcessPartStiffNumbers(pTHK_Process, pApp_Process, pUI_Process, pTHK_Observed, pApp_Observed, pUI_Observed, ref pClear, partNumberCells_1, partNumberCells_3, partNumberCells_5);
                pFlag = true;
            }

            // Process first as stiffener
            if (partStiff[0].Contains("S") && sTHK_Observed.Count == 0)
            {
                ProcessPartStiffNumbers(sTHK_Process, sApp_Process, sUI_Process, sTHK_Observed, sApp_Observed, sUI_Observed, ref sClear, partNumberCells_1, partNumberCells_3, partNumberCells_5);
                sFlag = true;
            }

            // Process second as partition
            if (partStiff.Contains("P") && pTHK_Observed.Count == 0 && !pFlag)
            {
                ProcessPartStiffNumbers(pTHK_Process, pApp_Process, pUI_Process, pTHK_Observed, pApp_Observed, pUI_Observed, ref pClear, partNumberCells_1, partNumberCells_3, partNumberCells_5);
            }

            // Process second as stiffener
            if (partStiff.Contains("S") && sTHK_Observed.Count == 0 && !sFlag)
            {
                ProcessPartStiffNumbers(sTHK_Process, sApp_Process, sUI_Process, sTHK_Observed, sApp_Observed, sUI_Observed, ref sClear, partNumberCells_1, partNumberCells_3, partNumberCells_5);
            }
        }
        private void ProcessPartStiffNumbers(List<double> thkProcess, List<IPropertyWrapper> appProcess, List<TextBox> uiProcess, List<double> thkObserved, List<IPropertyWrapper> appObserved, List<TextBox> uiObserved, ref bool clearFlag, List<string> partNumberCells0, List<string> partNumberCells1, List<string> partNumberCells2)
        {
            // Store the part number cell lists in an array or list for easy access
            List<string>[] partNumberCellsArray = { partNumberCells0, partNumberCells1, partNumberCells2 };

            for (int j = 0; j < 3; j++)
            {
                if (thkProcess[j] != 0)
                {
                    var currentPartNumberCells = partNumberCellsArray[j];
                    string value = CellString(BomInputSheet, currentPartNumberCells[0]);
                    appProcess[j].SetValue(value);
                    uiProcess[j].Text = value;
                    currentPartNumberCells.RemoveAt(0);
                    appProcess.Clear();
                    break;
                }
                else
                {
                    appProcess[j].SetValue("");
                    uiProcess[j].Text = "";
                }
            }

        }


        // Partition lists
        private List<double> InitializePTHKProcess_135()
        {
            return new List<double>
            {
                Header61.PartitionTHK,
                Header63.PartitionTHK,
                Header65.PartitionTHK,
            };
        }
        private List<double> InitializePTHKProcess_246()
        {
            return new List<double>
            {
                Header62.PartitionTHK,
                Header64.PartitionTHK,
                Header66.PartitionTHK,
            };
        }
        private List<IPropertyWrapper> InitializePAppProcess_135()
        {
            var wrappers = new List<IPropertyWrapper>();

            var tempWrappers = new[]
            {
                new { Getter = new Func<string>(() => Header61.PartitionPartNo), Setter = new Action<string>(value => Header61.PartitionPartNo = value) },
                new { Getter = new Func<string>(() => Header63.PartitionPartNo), Setter = new Action<string>(value => Header63.PartitionPartNo = value) },
                new { Getter = new Func<string>(() => Header65.PartitionPartNo), Setter = new Action<string>(value => Header65.PartitionPartNo = value) },
            };

            foreach (var wrapper in tempWrappers)
            {
                wrappers.Add(new PartitionPartNoWrapper(wrapper.Getter, wrapper.Setter));
            }

            return wrappers;
        }
        private List<IPropertyWrapper> InitializePAppProcess_246()
        {
            var wrappers = new List<IPropertyWrapper>();

            var tempWrappers = new[]
            {
                new { Getter = new Func<string>(() => Header62.PartitionPartNo), Setter = new Action<string>(value => Header62.PartitionPartNo = value) },
                new { Getter = new Func<string>(() => Header64.PartitionPartNo), Setter = new Action<string>(value => Header64.PartitionPartNo = value) },
                new { Getter = new Func<string>(() => Header66.PartitionPartNo), Setter = new Action<string>(value => Header66.PartitionPartNo = value) },
            };

            foreach (var wrapper in tempWrappers)
            {
                wrappers.Add(new PartitionPartNoWrapper(wrapper.Getter, wrapper.Setter));
            }

            return wrappers;
        }
        private List<TextBox> InitializePUIProcess_135()
        {
            return new List<TextBox>
            {
                tPartitionPartNo_61,
                tPartitionPartNo_63,
                tPartitionPartNo_65,
            };
        }
        private List<TextBox> InitializePUIProcess_246()
        {
            return new List<TextBox>
            {
                tPartitionPartNo_62,
                tPartitionPartNo_64,
                tPartitionPartNo_66,
            };
        }


        // Stiffener lists
        private List<double> InitializeSTHKProcess_135()
        {
            return new List<double>
            {
                Header61.StiffenerTHK,
                Header63.StiffenerTHK,
                Header65.StiffenerTHK,
            };
        }
        private List<double> InitializeSTHKProcess_246()
        {
            return new List<double>
            {
                Header62.StiffenerTHK,
                Header64.StiffenerTHK,
                Header66.StiffenerTHK,
            };
        }
        private List<IPropertyWrapper> InitializeSAppProcess_135()
        {
            var wrappers = new List<IPropertyWrapper>();

            var tempWrappers = new[]
            {
                new { Getter = new Func<string>(() => Header61.StiffenerPartNo), Setter = new Action<string>(value => Header61.StiffenerPartNo = value) },
                new { Getter = new Func<string>(() => Header63.StiffenerPartNo), Setter = new Action<string>(value => Header63.StiffenerPartNo = value) },
                new { Getter = new Func<string>(() => Header65.StiffenerPartNo), Setter = new Action<string>(value => Header65.StiffenerPartNo = value) },
            };

            foreach (var wrapper in tempWrappers)
            {
                wrappers.Add(new PartitionPartNoWrapper(wrapper.Getter, wrapper.Setter));
            }

            return wrappers;
        }
        private List<IPropertyWrapper> InitializeSAppProcess_246()
        {
            var wrappers = new List<IPropertyWrapper>();

            var tempWrappers = new[]
            {
                new { Getter = new Func<string>(() => Header62.StiffenerPartNo), Setter = new Action<string>(value => Header62.StiffenerPartNo = value) },
                new { Getter = new Func<string>(() => Header64.StiffenerPartNo), Setter = new Action<string>(value => Header64.StiffenerPartNo = value) },
                new { Getter = new Func<string>(() => Header66.StiffenerPartNo), Setter = new Action<string>(value => Header66.StiffenerPartNo = value) },
            };

            foreach (var wrapper in tempWrappers)
            {
                wrappers.Add(new PartitionPartNoWrapper(wrapper.Getter, wrapper.Setter));
            }

            return wrappers;
        }
        private List<TextBox> InitializeSUIProcess_135()
        {
            return new List<TextBox>
            {
                tStiffenerPartNo_61,
                tStiffenerPartNo_63,
                tStiffenerPartNo_65,
            };
        }
        private List<TextBox> InitializeSUIProcess_246()
        {
            return new List<TextBox>
            {
                tStiffenerPartNo_62,
                tStiffenerPartNo_64,
                tStiffenerPartNo_66,
            };
        }


        // Prego lookup cells
        private List<string> InitializePartNumberCells_1()
        {
            return new List<string>
            {
                "JF30", "JF32", "JF34", "JF36", "JF38", "JF40", "JF42", "JF44", "JF46", "JF48", "JF50", "JF52"
            };
        }
        private List<string> InitializePartNumberCells_2()
        {
            return new List<string>
            {
                "JF804", "JF806", "JF808", "JF810", "JF812", "JF814", "JF816", "JF818", "JF820", "JF822", "JF824", "JF826"
            };
        }
        private List<string> InitializePartNumberCells_3()
        {
            return new List<string>
            {
                "JF288", "JF290", "JF292", "JF294", "JF296", "JF298", "JF300", "JF302", "JF304", "JF306", "JF308", "JF310"
            };
        }
        private List<string> InitializePartNumberCells_4()
        {
            return new List<string>
            {
                "JF1062", "JF1064", "JF1066", "JF1068", "JF1070", "JF1072", "JF1074", "JF1076", "JF1078", "JF1080", "JF1082", "JF1084"
            };
        }
        private List<string> InitializePartNumberCells_5()
        {
            return new List<string>
            {
                "JF546", "JF548", "JF550", "JF552", "JF554", "JF556", "JF558", "JF560", "JF562", "JF564", "JF566", "JF568"
            };
        }
        private List<string> InitializePartNumberCells_6()
        {
            return new List<string>
            {
                "JF1320", "JF1322", "JF1324", "JF1326", "JF1328", "JF1330", "JF1332", "JF1334", "JF1336", "JF1338", "JF1340", "JF1342"
            };
        }


        // Property wrapper
        public interface IPropertyWrapper
        {
            string GetValue();
            void SetValue(string value);
        }
        public class PartitionPartNoWrapper : IPropertyWrapper
        {
            private Action<string> _setter;
            private Func<string> _getter;

            public PartitionPartNoWrapper(Func<string> getter, Action<string> setter)
            {
                _getter = getter;
                _setter = setter;
            }
            public string GetValue()
            {
                return _getter();
            }
            public void SetValue(string value)
            {
                _setter(value);
            }
        }

    }
}

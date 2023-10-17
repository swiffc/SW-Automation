using SolidWorks.Interop.sldworks;
using SolidWorks.Interop.swconst;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Walkway.Tools
{
    public class UserTools : Walkway
    {
        public static int CountHolesInAllSelectedFaces()
        {
            ModelDoc2 model = SW.ActiveDoc as ModelDoc2;
            if (model == null)
                return 0;

            SelectionMgr selectionManager = model.SelectionManager as SelectionMgr;
            if (selectionManager == null)
                return 0;

            int totalHoles = 0;
            int numSelected = selectionManager.GetSelectedObjectCount();

            for (int i = 1; i <= numSelected; i++)
            {
                Face2 selectedFace = selectionManager.GetSelectedObject6(i, -1) as Face2;
                if (selectedFace != null)
                {
                    totalHoles += CountHolesInFace(selectedFace);
                }
            }

            return totalHoles;
        }
        public static int CountCircularHolesInActiveDocument()
        {
            ModelDoc2 model = SW.ActiveDoc as ModelDoc2;

            if (model == null)
                return 0;

            if (model.GetType() == (int)swDocumentTypes_e.swDocPART)
            {
                return CountCircularHolesInPart(model as PartDoc);
            }
            else if (model.GetType() == (int)swDocumentTypes_e.swDocASSEMBLY)
            {
                AssemblyDoc assembly = model as AssemblyDoc;
                return CountCircularHolesInAssembly(assembly);
            }

            return 0; // Unsupported document type
        }
        private static int CountCircularHolesInPart(PartDoc part)
        {
            if (part == null)
                return 0;

            int totalHoleCount = 0;

            var bodies = part.GetBodies2((int)swBodyType_e.swSolidBody, false);
            foreach (Body2 body in bodies)
            {
                Face2 face = body.GetFirstFace();
                while (face != null)
                {
                    totalHoleCount += CountHolesInFace(face);
                    face = face.GetNextFace() as Face2;
                }
            }

            return totalHoleCount / 2;  // Dividing by 2 to account for through-holes
        }
        private static int CountCircularHolesInAssembly(AssemblyDoc assembly)
        {
            int totalHoleCount = 0;

            object[] components = assembly.GetComponents(false);
            foreach (Component2 component in components)
            {
                if (component.GetSuppression() != (int)swComponentSuppressionState_e.swComponentFullyResolved)
                    continue;

                ModelDoc2 componentModel = component.GetModelDoc2();
                if (componentModel != null && componentModel.GetType() == (int)swDocumentTypes_e.swDocPART)
                {
                    int partCount = CountCircularHolesInPart(componentModel as PartDoc);
                    Debug.WriteLine($"Component {component.Name2} contains {partCount} holes");
                    totalHoleCount += partCount;
                }
            }

            return totalHoleCount;
        }
        private static int CountHolesInFace(Face2 face)
        {
            int holeCount = 0;
            Loop2 loop = face.GetFirstLoop();

            while (loop != null)
            {
                if (IsHoleOrSlot(loop))
                {
                    holeCount++;
                }

                loop = loop.GetNext() as Loop2;
            }

            return holeCount;
        }
        private static bool IsHoleOrSlot(Loop2 loop)
        {
            if (loop.IsOuter())
                return false;

            int edgeCount = loop.GetEdgeCount();

            // Checking for circular holes
            if (edgeCount == 1)
            {
                CoEdge coEdge = loop.GetFirstCoEdge();
                Edge edge = coEdge.GetEdge() as Edge;
                Curve curve = edge.GetCurve();
                return curve.IsCircle();
            }

            // Checking for elongated slots with rounded ends
            else if (edgeCount >= 4)
            {
                int circleCount = 0;
                int straightCount = 0;
                CoEdge coEdge = loop.GetFirstCoEdge();
                for (int i = 0; i < edgeCount; i++)
                {
                    Edge edge = coEdge.GetEdge() as Edge;
                    Curve curve = edge.GetCurve();
                    if (curve.IsCircle() || curve.IsEllipse())
                    {
                        circleCount++;
                    }
                    else if (curve.IsLine())
                    {
                        straightCount++;
                    }
                    coEdge = coEdge.GetNext() as CoEdge;
                }

                // If the loop contains 2 circle/ellipse edges (rounded ends) and the remaining are straight edges
                return circleCount == 2 && (edgeCount - circleCount) == straightCount;
            }

            return false; // Any other case is not considered as a hole
        }
    }
}

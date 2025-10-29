
using System.Collections.Generic;
using SolidWorks.Interop.sldworks;

namespace UnifiedUI.Services.SolidWorksApi
{
    /// <summary>
    /// Interface for SolidWorks drawing automation operations
    /// </summary>
    public interface ISolidWorksDrawingService
    {
        /// <summary>
        /// Creates a new drawing from a template
        /// </summary>
        /// <param name="templatePath">Path to drawing template</param>
        /// <returns>Drawing document or null if failed</returns>
        DrawingDoc CreateDrawing(string templatePath);

        /// <summary>
        /// Inserts a model view into the drawing
        /// </summary>
        /// <param name="modelPath">Path to model file</param>
        /// <param name="x">X coordinate</param>
        /// <param name="y">Y coordinate</param>
        /// <param name="scale">View scale</param>
        /// <returns>True if successful</returns>
        bool InsertModelView(string modelPath, double x, double y, double scale = 1.0);

        /// <summary>
        /// Creates standard 3 views (Front, Top, Right)
        /// </summary>
        /// <param name="modelPath">Path to model file</param>
        /// <returns>True if successful</returns>
        bool CreateStandard3Views(string modelPath);

        /// <summary>
        /// Adds dimensions to selected entities
        /// </summary>
        /// <returns>True if successful</returns>
        bool AddDimensions();

        /// <summary>
        /// Gets all sheets in the drawing
        /// </summary>
        /// <returns>List of sheet names</returns>
        List<string> GetAllSheets();

        /// <summary>
        /// Activates a specific sheet
        /// </summary>
        /// <param name="sheetName">Name of the sheet</param>
        /// <returns>True if successful</returns>
        bool ActivateSheet(string sheetName);

        /// <summary>
        /// Adds a new sheet to the drawing
        /// </summary>
        /// <param name="sheetName">Name for the new sheet</param>
        /// <returns>True if successful</returns>
        bool AddSheet(string sheetName);
    }
}

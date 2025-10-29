
using System.Collections.Generic;
using SolidWorks.Interop.sldworks;

namespace UnifiedUI.Services.SolidWorksApi
{
    /// <summary>
    /// Interface for SolidWorks assembly operations
    /// </summary>
    public interface ISolidWorksAssemblyService
    {
        /// <summary>
        /// Creates a new assembly from template
        /// </summary>
        /// <param name="templatePath">Path to assembly template</param>
        /// <returns>Assembly document or null if failed</returns>
        AssemblyDoc CreateAssembly(string templatePath);

        /// <summary>
        /// Inserts a component into the assembly
        /// </summary>
        /// <param name="componentPath">Path to component file</param>
        /// <param name="x">X coordinate</param>
        /// <param name="y">Y coordinate</param>
        /// <param name="z">Z coordinate</param>
        /// <returns>Inserted component or null if failed</returns>
        Component2 InsertComponent(string componentPath, double x = 0, double y = 0, double z = 0);

        /// <summary>
        /// Mates two components
        /// </summary>
        /// <param name="entity1">First entity to mate</param>
        /// <param name="entity2">Second entity to mate</param>
        /// <param name="mateType">Type of mate (coincident, parallel, etc.)</param>
        /// <returns>True if successful</returns>
        bool AddMate(object entity1, object entity2, int mateType);

        /// <summary>
        /// Gets all components in the assembly
        /// </summary>
        /// <param name="includeHidden">Include hidden components</param>
        /// <returns>List of components</returns>
        List<Component2> GetAllComponents(bool includeHidden = false);

        /// <summary>
        /// Suppresses a component
        /// </summary>
        /// <param name="componentName">Name of the component</param>
        /// <returns>True if successful</returns>
        bool SuppressComponent(string componentName);

        /// <summary>
        /// Unsuppresses a component
        /// </summary>
        /// <param name="componentName">Name of the component</param>
        /// <returns>True if successful</returns>
        bool UnsuppressComponent(string componentName);

        /// <summary>
        /// Replaces a component with another file
        /// </summary>
        /// <param name="componentName">Name of component to replace</param>
        /// <param name="newComponentPath">Path to new component file</param>
        /// <returns>True if successful</returns>
        bool ReplaceComponent(string componentName, string newComponentPath);

        /// <summary>
        /// Packs and goes the assembly (collects all referenced files)
        /// </summary>
        /// <param name="outputFolder">Folder to save files</param>
        /// <returns>True if successful</returns>
        bool PackAndGo(string outputFolder);
    }
}

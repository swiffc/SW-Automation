using System; 
using System.Windows.Input; 

namespace UserInterface.Core
{
    class RelayCommand : ICommand
    {
        private Action<object> _execute;
        private Func<object, bool> _canExecute;

        public event EventHandler CanExecuteChanged
        {
            // Add and remove event handlers to CommandManager's RequerySuggested
            // This allows the command to automatically requery the CanExecute method
            // when the CommandManager thinks something has changed that might affect it
            add { CommandManager.RequerySuggested += value; }
            remove { CommandManager.RequerySuggested -= value; }
        }

        // Constructor that initializes the RelayCommand with the execute logic and optionally canExecute logic
        public RelayCommand(Action<object> execute, Func<object, bool> canExecute = null)
        {
            // Save the actions provided by the user
            _execute = execute ?? throw new ArgumentNullException(nameof(execute));
            _canExecute = canExecute;
        }

        public bool CanExecute(object parameter)
        {
            return _canExecute == null || _canExecute(parameter);
        }

        public void Execute(object parameter)
        {
            _execute(parameter);
        }
    }
}

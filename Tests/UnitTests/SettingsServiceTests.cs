
using System;
using System.IO;
using Xunit;
using FluentAssertions;
using UnifiedUI.Models.Configuration;
using UnifiedUI.Services.Configuration;

namespace SolidWorksAutomation.Tests.UnitTests
{
    /// <summary>
    /// Unit tests for SettingsService
    /// </summary>
    public class SettingsServiceTests : IDisposable
    {
        private readonly string _testSettingsFile;
        private readonly SettingsService _settingsService;

        public SettingsServiceTests()
        {
            _testSettingsFile = Path.Combine(Path.GetTempPath(), $"test_settings_{Guid.NewGuid()}.json");
            _settingsService = new SettingsService(_testSettingsFile);
        }

        [Fact]
        public void GetSettings_ShouldReturnDefaultSettings_WhenNoSettingsFileExists()
        {
            // Act
            var settings = _settingsService.GetSettings();

            // Assert
            settings.Should().NotBeNull();
            settings.Paths.Should().NotBeNull();
            settings.SolidWorks.Should().NotBeNull();
            settings.Workflows.Should().NotBeNull();
        }

        [Fact]
        public void SaveSettings_ShouldReturnTrue_WhenSettingsAreValid()
        {
            // Arrange
            var settings = new AppSettings();

            // Act
            var result = _settingsService.SaveSettings(settings);

            // Assert
            result.Should().BeTrue();
            File.Exists(_testSettingsFile).Should().BeTrue();
        }

        [Fact]
        public void SaveSettings_ShouldReturnFalse_WhenSettingsAreNull()
        {
            // Act
            var result = _settingsService.SaveSettings(null);

            // Assert
            result.Should().BeFalse();
        }

        [Fact]
        public void ResetToDefaults_ShouldReturnDefaultSettings()
        {
            // Act
            var settings = _settingsService.ResetToDefaults();

            // Assert
            settings.Should().NotBeNull();
            settings.SolidWorks.AutoStartSolidWorks.Should().BeTrue();
            settings.Workflows.EnableAutoRetry.Should().BeTrue();
        }

        [Fact]
        public void ValidateSettings_ShouldReturnValid_WhenSettingsAreCorrect()
        {
            // Arrange
            var settings = new AppSettings();

            // Act
            var result = _settingsService.ValidateSettings(settings);

            // Assert
            result.IsValid.Should().BeTrue();
            result.Errors.Should().BeEmpty();
        }

        [Fact]
        public void ValidateSettings_ShouldReturnInvalid_WhenTimeoutsAreNegative()
        {
            // Arrange
            var settings = new AppSettings
            {
                SolidWorks = new SolidWorksSettings
                {
                    DocumentTimeout = -1
                }
            };

            // Act
            var result = _settingsService.ValidateSettings(settings);

            // Assert
            result.IsValid.Should().BeFalse();
            result.Errors.Should().Contain(e => e.Contains("timeout"));
        }

        [Fact]
        public void GetSetting_ShouldReturnDefaultValue_WhenSettingNotFound()
        {
            // Act
            var value = _settingsService.GetSetting<string>("NonExistentKey", "default");

            // Assert
            value.Should().Be("default");
        }

        [Fact]
        public void SetSetting_ShouldSaveValue_AndBeRetrievable()
        {
            // Arrange
            var key = "TestKey";
            var value = "TestValue";

            // Act
            _settingsService.SetSetting(key, value);
            var retrieved = _settingsService.GetSetting<string>(key);

            // Assert
            retrieved.Should().Be(value);
        }

        public void Dispose()
        {
            // Cleanup
            if (File.Exists(_testSettingsFile))
            {
                File.Delete(_testSettingsFile);
            }
        }
    }
}

package scala.tools.eclipse.properties

import scala.tools.nsc.Settings
import scala.tools.eclipse.{ SettingConverterUtil }
import org.eclipse.swt.widgets.{ Button, Combo, Composite, Control, Event, Group, Label, Listener, Text }
import org.eclipse.swt.layout.{ GridData, GridLayout }
import org.eclipse.swt.SWT
import org.eclipse.swt.events.{ ModifyEvent, ModifyListener, SelectionAdapter, SelectionEvent, SelectionListener }
import org.eclipse.jface.preference.IPreferenceStore
import java.io.File
import org.eclipse.jface.fieldassist.FieldDecorationRegistry
import org.eclipse.jface.fieldassist.ControlDecoration
import scala.tools.eclipse.ScalaPlugin
import java.net.URI
import scala.tools.eclipse.util.SWTUtils

trait EclipseSettings {
  self: ScalaPluginPreferencePage =>

  object EclipseSetting {
    /** Function to map a Scala compiler setting to an Eclipse plugin setting */
    def apply(setting: Settings#Setting): EclipseSetting = setting match {
      case setting: Settings#BooleanSetting => new CheckBoxSetting(setting)
      case setting: Settings#IntSetting     => new IntegerSetting(setting)
      case setting: Settings#StringSetting =>
        setting.name match {
          case "-Ypresentation-log" | "-Ypresentation-replay" =>
            new FileSetting(setting)
          case _ =>
            new StringSetting(setting)
        }
      //    case setting : Settings#PhasesSetting  => new StringSetting(setting) // !!!
      case setting: Settings#MultiStringSetting =>
        setting.name match {
          case "-Xplugin" => new MultiFileSetting(setting)
          case _          => new MultiStringSetting(setting)
        }
      case setting: Settings#ChoiceSetting => new ComboSetting(setting)
    }

    case class EclipseBox(name: String, eSettings: List[EclipseSetting])
    def toEclipseBox(userBox: IDESettings.Box, preferenceStore: IPreferenceStore): EclipseBox = {
      val eSettings = userBox.userSettings.map { s: Settings#Setting =>
        val name = SettingConverterUtil.convertNameToProperty(s.name)
        s.tryToSetFromPropertyValue(preferenceStore.getString(name))
        EclipseSetting(s)
      }
      EclipseBox(userBox.name, eSettings)
    }
  }

  private object SelectionListenerSing extends SelectionAdapter {
    override def widgetSelected(e: SelectionEvent): Unit = updateApply
  }

  private object ModifyListenerSing extends ModifyListener {
    def modifyText(e: ModifyEvent) = updateApply
  }

  /** Represents a setting that may by changed within Eclipse.
   */
  abstract class EclipseSetting(val setting: Settings#Setting) {
    def control: Control
    val data = new GridData()
    data.horizontalAlignment = GridData.FILL

    def setEnabled(value: Boolean): Unit = {
      control.setEnabled(value)
      if (!value) {
        reset
      }
    }

    def addTo(page: Composite) {
      val label = new Label(page, SWT.NONE)
      label.setText(SettingConverterUtil.convertNameToProperty(setting.name))
      createControl(page)
      val help = new Label(page, SWT.WRAP)
      help.setText(setting.helpDescription)
    }

    /** Create the control on the page */
    def createControl(page: Composite)
    def isChanged: Boolean

    /** Reset the control to a default value */
    def reset()

    /** Apply the value of the control */
    def apply()
  }

  /** Boolean setting controlled by a checkbox.
   */
  class CheckBoxSetting(setting: Settings#BooleanSetting)
    extends EclipseSetting(setting) {
    var control: Button = _

    def createControl(page: Composite) {
      control = new Button(page, SWT.CHECK)
      control.setSelection(setting.value)
      control.addSelectionListener(
        SelectionListenerSing)
    }

    def isChanged = !setting.value.equals(control.getSelection)

    def reset() { control.setSelection(false) }

    def apply() { setting.value = control.getSelection }
  }

  /** Integer setting editable using a text field.
   */
  class IntegerSetting(setting: Settings#IntSetting)
    extends EclipseSetting(setting) {
    var control: Text = _

    def createControl(page: Composite) {
      control = new Text(page, SWT.SINGLE | SWT.BORDER)
      control.setLayoutData(data)
      control.setText(setting.value.toString)
      control.addListener(SWT.Verify, new Listener {
        def handleEvent(e: Event) { if (e.text.exists(c => c < '0' || c > '9')) e.doit = false }
      })
      control.addModifyListener(ModifyListenerSing)
    }

    def isChanged = setting.value.toString != control.getText

    def reset() { control.setText(setting.default.toString) }
    def apply() {
      setting.value = try {
        control.getText.toInt
      } catch {
        case _: NumberFormatException => setting.default
      }
    }
  }

  /** String setting editable using a text field.
   */
  class StringSetting(setting: Settings#StringSetting)
    extends EclipseSetting(setting) {
    var control: Text = _
    def createControl(page: Composite) {
      control = new Text(page, SWT.SINGLE | SWT.BORDER)
      control.setText(setting.value)
      var layout = data
      if (setting.value.isEmpty) {
        layout = new GridData()
        layout.widthHint = 100
      }
      control.setLayoutData(layout)
      control.addModifyListener(ModifyListenerSing)
    }

    def isChanged = setting.value != control.getText
    def reset() { control.setText(setting.default) }
    def apply() { setting.value = control.getText }
  }

  /** Multi string setting editable using a text field.
   */
  class MultiStringSetting(setting: Settings#MultiStringSetting)
    extends EclipseSetting(setting) {
    var control: Text = _
    def createControl(page: Composite) {
      control = new Text(page, SWT.SINGLE | SWT.BORDER)
      control.setLayoutData(data)
      control.setText(setting.value.mkString(" "))
      control.addModifyListener(ModifyListenerSing)
    }

    def isChanged = setting.value.mkString(" ") != control.getText
    def reset() { control.setText("") }
    def apply() { setting.value = control.getText.trim.split(" +").toList }
  }

  /** Text setting selectable using a drop down combo box.
   */
  class ComboSetting(setting: Settings#ChoiceSetting)
    extends EclipseSetting(setting) {
    var control: Combo = _
    def createControl(page: Composite) {
      control = new Combo(page, SWT.DROP_DOWN | SWT.READ_ONLY)
      control.setLayoutData(data)
      setting.choices.foreach(control.add)
      control.setText(setting.value)
      control.addSelectionListener(SelectionListenerSing)
    }

    def isChanged = setting.value != control.getText
    def reset() { control.setText(setting.default) }
    def apply() { setting.value = control.getText }
  }

  import scala.tools.eclipse.util.FileUtils._
  
  /** String setting editable using a File dialog.
   *
   *  @note Temporary implementation. This one does not have a File dialog, instead
   *       it prefixes the workspace path when the filename is not an absolute path.
   */
  class FileSetting(setting: Settings#StringSetting)
    extends EclipseSetting(setting) {
    var control: Text = _
    
    def createControl(page: Composite) {
      control = new Text(page, SWT.SINGLE | SWT.BORDER)
      control.setText(setting.value)
      var layout = data
      if (setting.value.isEmpty) {
        layout = new GridData()
        layout.widthHint = 200
      }
      control.setLayoutData(layout)
      control.setMessage("Path is relative to the workspace")
      control.addModifyListener(ModifyListenerSing)
    }

    def originalString: String = control.getText
    
    def isChanged = setting.value != absoluteFileName(control.getText)
    def reset() { control.setText(setting.default) }
    def apply() { setting.value = absoluteFileName(control.getText) }
  }

  class MultiFileSetting(setting: Settings#MultiStringSetting) extends EclipseSetting(setting) {
    var control: Text = _
    
    lazy val errorIndicator = FieldDecorationRegistry.getDefault().getFieldDecoration(FieldDecorationRegistry.DEC_ERROR)

    lazy val errorDecoration: ControlDecoration = {
      val decoration = new ControlDecoration(control, SWT.TOP | SWT.LEFT)
      decoration.setImage(errorIndicator.getImage())
      decoration.setDescriptionText(errorIndicator.getDescription())
      decoration
    }

    
    def createControl(page: Composite) {
      control = new Text(page, SWT.SINGLE | SWT.BORDER)
      control.setText(setting.value.mkString(", "))
      
      lazy val variableManager = ScalaPlugin.plugin.workspaceRoot.getPathVariableManager()
      
      var layout = data
      if (setting.value.isEmpty) {
        layout = new GridData()
        layout.widthHint = 200
      }
      control.setLayoutData(layout)
      control.setMessage("Path is relative to the workspace")
      control.addModifyListener(ModifyListenerSing)
      
      import SWTUtils._
      
      control.addModifyListener { (event: ModifyEvent) =>
        val errors = new StringBuffer
        
        val paths = control.getText().split(File.pathSeparatorChar).map(_.trim)
        for (p <- paths) {
          val internalPath = variableManager.convertFromUserEditableFormat(p, false)
          val resolved = variableManager.resolveURI(new URI(internalPath))
          if (!new File(resolved).exists)
            errors.append("Could not find %s".format(p))
        }
        
        if (errors.length() > 0) {
          errorDecoration.setDescriptionText(errors.toString)
        } else {
          errorDecoration.show()
        }
      }
    }
    
    def fileNames() = {
      control.getText().split(File.pathSeparatorChar).map(f => absoluteFileName(f.trim)).toList
    }
    
    def originalFileNames() =
      control.getText()

    override def isChanged = setting.value != fileNames()
    override def reset() { control.setText("") }
    override def apply() { setting.value = fileNames() }
  }
}

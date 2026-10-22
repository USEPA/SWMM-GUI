"""Source-level regression checks; these do NOT replace Delphi/VCL GUI tests."""
from pathlib import Path
import re
import unittest

SOURCE = Path(__file__).resolve().parents[1] / 'Epaswmm5'


def read(name):
    return (SOURCE / name).read_text(encoding='utf-8-sig')


def method(name):
    source = read('Fmap.pas')
    start = source.index('procedure TMapForm.' + name)
    end = source.find('\nprocedure ', start + 1)
    return source[start:end if end != -1 else None]


class OutletPickerSourceContract(unittest.TestCase):
    def test_outlet_gets_editable_button_only(self):
        props = read('objprops.txt')
        self.assertRegex(props, r"Name:'Outlet';\s+Style:esEditButton")
        self.assertEqual(props.count('Style:esEditButton'), 1)

    def test_button_displays_plus_and_manual_edit_remains(self):
        component = read('PropEdit.pas')
        self.assertIn("DrawText(aCanvas.Handle, '+', 1, R,", component)
        self.assertIn('Style in [esEdit, esEditButton]', component)
        self.assertIn('Width := Width - FGrid.DefaultRowHeight', component)
        self.assertIn('(Key = VK_F4)', component)

    def test_property_button_starts_map_selection(self):
        self.assertIn('MapForm.BeginOutletSelection(Project.CurrentItem[SUBCATCH])',
                      read('Fproped.pas'))

    def test_cancel_does_not_write_model(self):
        body = method('CancelOutletSelection')
        for forbidden in ['.Data[', 'HasChanged :=', 'UpdateSubcatchOutlet(']:
            self.assertNotIn(forbidden, body)
        self.assertIn('OutletSubcatch := nil', body)
        self.assertIn('Cursor := OutletPreviousCursor', body)

    def test_only_native_node_hit_test_can_assign_outlet(self):
        body = method('PickOutletNode')
        self.assertIn('if not FindNode(X, Y) then', body)
        self.assertNotIn('FindSubcatch(', body)
        self.assertIn('finally\n    SelectedObject := OldObject;', body)
        self.assertIn('SelectedIndex := OldIndex;', body)

    def test_target_is_checked_before_dereference(self):
        body = method('PickOutletNode')
        self.assertLess(body.index('IndexOfObject(C)'), body.index('C.Data['))
        self.assertIn('(EditorObject <> SUBCATCH)', body)
        self.assertIn('(EditorIndex <> Index)', body)

    def test_native_connection_and_dirty_state_are_updated(self):
        body = method('PickOutletNode')
        self.assertLess(body.index('Uupdate.UpdateSubcatchOutlet(C, S, Errmsg)'),
                        body.index('C.Data[SUBCATCH_OUTLET_INDEX] := S'))
        self.assertIn('HasChanged := True;', body)
        self.assertIn('Editor.SetProp(SUBCATCH_OUTLET_INDEX, S)', body)

    def test_picking_cannot_start_drag_or_delete(self):
        down = method('FormMouseDown')
        self.assertLess(down.index('if Button = mbLeft then Exit;'),
                        down.index('BeginSelecting(X, Y)'))
        keyboard = method('FormKeyDown')
        self.assertLess(keyboard.index('if OutletSubcatch <> nil then'),
                        keyboard.index('VK_DELETE'))
        self.assertIn('if Key = VK_ESCAPE then CancelOutletSelection', keyboard)
        self.assertIn('if Button = mbRight then', down)

    def test_switching_object_tool_or_project_cancels_pick(self):
        for name in ['ClearMap', 'ToolButtonClick', 'FormClose']:
            self.assertIn('CancelOutletSelection;', method(name))
        self.assertIn('MapForm.CancelOutletSelection;', read('Uedit.pas'))


if __name__ == '__main__':
    unittest.main()

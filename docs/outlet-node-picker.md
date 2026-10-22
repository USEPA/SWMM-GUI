# Select a subcatchment outlet node on the map

This unofficial GUI enhancement adds a **+** button beside the subcatchment
**Outlet** property. It does not change the SWMM solver or the INP format.

## Usage

1. Open the property editor for a subcatchment and select **Outlet**.
2. Click **+**, or press **F4** while the property grid has focus.
3. Click a visible Junction, Outfall, Divider or Storage node on the map.
4. Save the project as usual.

The existing outlet is unchanged until a node is picked. **Esc** or a right-click
cancels. Clicking empty space keeps the picker active. Changing the edited
object, map tool or project cancels the pending selection. Typing an outlet ID
manually remains supported, including the existing subcatchment-to-subcatchment
routing workflow.

Assignment reuses SWMM's node hit test and `UpdateSubcatchOutlet`, updates the
property editor, and marks the project as changed. No objects are moved and no
outlet is assigned automatically by nearest distance.

## Implementation

- `PropEdit.pas`: editable text field with a map-selection button.
- `objprops.txt` and `Fproped.pas`: expose and activate the picker for Outlet.
- `Fmap.pas`: asynchronous node picking, cancellation, and target checks.
- `Uedit.pas`: cancel a pending pick when the edited object changes.

## Verification

The modified GUI was compiled as a 64-bit Windows application in the Delphi 13
Community Edition IDE. A user confirmed successful interactive operation.
The solver was not modified. No hydraulic simulation was used to validate this
GUI-only change.

Run source-level checks with:

```text
python -m unittest discover -s tests -v
```

These checks inspect source contracts only. They do not compile Pascal or
replace VCL GUI regression tests. Build prerequisites are described in the
upstream `README.md` and `Components/Installation.txt`.

Before merging upstream, manually verify:

- Picking each supported node type, including replacing an existing outlet.
- Esc/right-click cancellation without changing the old outlet or dirty state.
- Clicking a link, subcatchment centroid or empty space does not assign or move it.
- Choosing the existing outlet does not create a new dirty state.
- Switching object/project or deleting the target cancels safely.
- Manual ID entry, invalid-ID validation, save and reopen.
- Zoom, middle-button panning, different DPI settings, and other property buttons.

This is a local community contribution, not an official EPA release or
endorsement. Acceptance into the official GUI is up to its maintainers.

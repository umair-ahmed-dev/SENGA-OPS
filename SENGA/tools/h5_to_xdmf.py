#!/usr/bin/env python3
"""
h5_to_xmf.py — Generate an XMF/XDMF wrapper for a 3D HDF5 file so it opens in ParaView.
Assumes 3D *cell-centered* datasets of identical shape (Nz, Ny, Nx) unless you override --axis-order.

Usage:
  python h5_to_xmf.py path/to/file.h5 [--origin 0 0 0] [--spacing 1 1 1] [--axis-order z y x] [--name GRIDNAME]
"""
import argparse, sys, os
from typing import List, Tuple
try:
    import h5py
except Exception as e:
    print("h5py is required. Install with: pip install h5py", file=sys.stderr); raise

def find_3d_fields(h5):
    """Return list of (path, dtype, shape) for 3D datasets under root that share the dominant shape."""
    fields = []
    def visit(name, obj):
        if isinstance(obj, h5py.Dataset) and obj.ndim == 3:
            fields.append((name, str(obj.dtype), tuple(obj.shape)))
    h5.visititems(visit)
    if not fields:
        raise RuntimeError("No 3D datasets found.")
    # choose the most common shape
    from collections import Counter
    shape_counts = Counter([f[2] for f in fields])
    main_shape, _ = shape_counts.most_common(1)[0]
    fields = [f for f in fields if f[2] == main_shape]
    return main_shape, fields

def dims_for_xdmf(shape: Tuple[int,int,int], axis_order: Tuple[str,str,str]):
    """Return (Nz, Ny, Nx) consistent with XDMF expectations given the on-disk axis order."""
    ao = tuple(a.lower() for a in axis_order)
    if sorted(ao) != ['x','y','z']:
        raise ValueError(f"Invalid --axis-order {axis_order}, expected a permutation of x y z")
    perm = [ao.index(c) for c in ('z','y','x')]
    Nz, Ny, Nx = shape[perm[0]], shape[perm[1]], shape[perm[2]]
    return Nz, Ny, Nx

def make_xmf(h5path: str, grid_name: str, datasets: List[Tuple[str,str,Tuple[int,int,int]]],
             Nz: int, Ny: int, Nx: int, origin, spacing):
    """Return XMF text for a Uniform 3DCoRectMesh with cell-centered attributes pointing into the H5 file."""
    def attribute(name, dtype, dims):
        nd = dtype.lower()
        number_type = "Float" if ("float" in nd or "f" in nd) else ("Int" if "int" in nd else "Float")
        precision = "8" if ("64" in nd or nd.endswith("f8")) else ("4" if ("32" in nd or nd.endswith("f4")) else "")
        prec_attr = f' Precision="{precision}"' if precision else ""
        return (
            f'    <Attribute Name="{name}" AttributeType="Scalar" Center="Cell">\n'
            f'      <DataItem Format="HDF" Dimensions="{Nz} {Ny} {Nx}" NumberType="{number_type}"{prec_attr}>{os.path.basename(h5path)}:/{name}</DataItem>\n'
            f'    </Attribute>'
        )
    attrs = "\n".join(attribute(name, dtype, shape) for (name, dtype, shape) in datasets)
    ox, oy, oz = origin
    dx, dy, dz = spacing
    xmf = f'''<?xml version="1.0" ?>
<Xdmf Version="3.0">
  <Domain>
    <Grid Name="{grid_name}" GridType="Uniform">
      <Topology TopologyType="3DCoRectMesh" Dimensions="{Nz+1} {Ny+1} {Nx+1}"/>
      <Geometry GeometryType="ORIGIN_DXDYDZ">
        <DataItem Dimensions="3" NumberType="Float" Precision="8" Format="XML">{ox} {oy} {oz}</DataItem>
        <DataItem Dimensions="3" NumberType="Float" Precision="8" Format="XML">{dx} {dy} {dz}</DataItem>
      </Geometry>
{attrs}
    </Grid>
  </Domain>
</Xdmf>
'''
    return xmf

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("h5file", help="Path to the input HDF5 file")
    ap.add_argument("--origin", type=float, nargs=3, default=(0.0, 0.0, 0.0), metavar=("OX","OY","OZ"))
    ap.add_argument("--spacing", type=float, nargs=3, default=(1.0, 1.0, 1.0), metavar=("DX","DY","DZ"))
    ap.add_argument("--axis-order", nargs=3, default=("z","y","x"), help="Order of axes in the HDF5 datasets, e.g. 'z y x' (default) or 'x y z'")
    ap.add_argument("--name", default="GRID", help="Name of the XDMF grid")
    args = ap.parse_args()

    import h5py
    with h5py.File(args.h5file, "r") as h5:
        main_shape, fields = find_3d_fields(h5)

    Nz, Ny, Nx = dims_for_xdmf(main_shape, tuple(args.axis_order))
    xmf_text = make_xmf(args.h5file, args.name, fields, Nz, Ny, Nx, args.origin, args.spacing)

    outpath = os.path.splitext(args.h5file)[0] + ".xmf"
    with open(outpath, "w") as f:
        f.write(xmf_text)
    print(f"Wrote {outpath} with {len(fields)} fields; grid dims (cells) = {Nz} x {Ny} x {Nx}")

if __name__ == "__main__":
    main()

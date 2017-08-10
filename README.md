# Nequick-ITUR
python wrapper for Nequick-ITUR

Nequick-ITUR is an ionospheric model accepted by the International Telecommunication Union (ITU)

The fortran code can be downloaded from <http://www.itu.int/en/ITU-R/study-groups/rsg3/Pages/iono-tropo-spheric.aspx>

---
## Why this repository?
Out of the box, the fortran library does not compile well with `f2py` on Ubuntu 16. This repository describes and saves the modifications required

## Modification
1. Rename `CCIR*.ASC` to lowercase `ccir*.asc`
1. Rename uppercase `DIPLATS.ASC` to lowercase `diplats.asc`
2. Remove invalid end of file character in the following fortran source files
    * NeQuick_ITU.for
    * eldens_ITUR.for
    * slQu.for

3. Generate `f2py` signature file for NeQuick

        ```bash
        f2py NeQuick_ITUR.for -m NeQuick_ITUR -h NeQuick_ITUR.pyf
        ```
    
4. Remove the following lines from the function signature

        ```
                entry eldens(h,alat,along)
                entry vert(h)
        ```
optionally you may want to define two new function/subroutine signatures of the two entry points you have deleted

5. Generate `*.so` files

        ```bash
        f2py -c NeQuick_ITUR.pyf NeQuick_ITUR.for
        f2py -c slQu.for -m slQu
        f2py -c eldens_ITUR.for -m eldens_ITUR
        ```
        
6. Test in python:

        ```python
        import NeQuick_ITUR
        Nequick_ITUR.nequick(100,40,40,12,100,12)
        ```

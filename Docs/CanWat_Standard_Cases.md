## Case 01

### Dynamic Driver

Purely artificial data set starting 01.06.2050  10:09:00 with timesteps of 10 min over 6 hours (going to 01.06.2050  17:59:00)

constant meteorological conditions

| T1100 | u1100 | DD1100 | Rn1100 | Rg1100 | Pa     |
| ----- | ----- | ------ | ------ | ------ | ------ |
| 20    | 3     | 15     | 130    | 165    | 101200 |



### Static Drivers

the trees are "created" with
d:\0_ownCloud_robq\0_R_lib\s_Vegetation\Baum.R

It is a voxel interpretation of the Plant Area Density (PAD) and has a resolution in x,y,z  of 1m, 1m, 1m, and the dimensions dim=c(x,y,z)

- Case01_tree_1D: x =   1, y =   1, z = 10
- Case01_tree_3D: x = 10, y = 10, z = 10
- Case01_tree_3D: x = 10, y = 10, z = 10
- Case02_tree_1D: x =   1, y =   1, z = 20
- Case02_tree_3D: x = 20, y = 20, z = 20

It is stored as one dimensional vektor with highest frequency in z, then y and x.
Thus z is the most inner loop and x is the outer loop.


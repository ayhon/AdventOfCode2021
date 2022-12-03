val inputs = List(156, 176, 175, 176, 183, 157, 150, 153, 154, 170, 162, 167, 170, 188, 190, 194, 196, 198, 202, 203, 187, 189, 194, 213, 216, 217, 224, 217, 216, 224, 237, 251, 259, 260, 292, 312, 313, 319, 315, 316, 324, 330, 331, 346, 361, 388, 373, 363, 338, 342, 337, 331, 333, 328, 329, 335, 336, 334, 332, 333, 337, 338, 345, 344, 342, 345, 344, 320, 343, 346, 347, 339, 363, 349, 362, 359, 373, 362, 363, 356, 360, 367, 368, 369, 370, 362, 366, 367, 382, 376, 393, 404, 403, 405, 404, 437, 447, 448, 447, 448, 456, 457, 459, 461, 459, 462, 450, 452, 453, 461, 469, 464, 470, 474, 478, 497, 500, 524, 527, 522, 525, 526, 524, 527, 547, 548, 549, 532, 518, 555, 558, 556, 575, 586, 585, 596, 592, 593, 583, 584, 598, 604, 626, 629, 635, 636, 641, 644, 646, 639, 643, 617, 616, 617, 619, 629, 630, 625, 624, 628, 636, 638, 639, 658, 665, 670, 667, 641, 645, 653, 680, 689, 665, 640, 633, 635, 641, 632, 635, 637, 638, 641, 653, 661, 671, 679, 678, 675, 677, 681, 682, 703, 704, 705, 710, 712, 719, 722, 709, 710, 712, 715, 721, 719, 720, 750, 739, 717, 714, 715, 716, 723, 722, 720, 724, 723, 717, 708, 725, 723, 709, 715, 722, 711, 694, 695, 702, 700, 724, 721, 720, 749, 748, 749, 754, 756, 763, 773, 753, 755, 754, 758, 761, 762, 760, 762, 765, 768, 788, 812, 813, 814, 828, 833, 848, 851, 853, 858, 863, 865, 866, 864, 863, 865, 866, 879, 888, 889, 912, 917, 910, 912, 919, 924, 913, 927, 928, 927, 931, 939, 943, 946, 948, 952, 951, 952, 965, 968, 970, 964, 946, 947, 952, 954, 962, 965, 967, 981, 982, 992, 993, 995, 997, 987, 990, 1005, 1018, 1000, 999, 1000, 1001, 983, 978, 1004, 999, 1002, 1024, 1022, 1030, 1037, 1061, 1071, 1081, 1083, 1077, 1076, 1086, 1101, 1116, 1137, 1153, 1158, 1160, 1167, 1168, 1184, 1204, 1214, 1250, 1271, 1273, 1272, 1274, 1297, 1303, 1304, 1331, 1349, 1350, 1355, 1356, 1357, 1358, 1371, 1391, 1396, 1395, 1399, 1367, 1368, 1330, 1327, 1335, 1336, 1340, 1331, 1332, 1329, 1337, 1324, 1325, 1326, 1327, 1313, 1326, 1328, 1331, 1307, 1309, 1310, 1320, 1324, 1325, 1324, 1353, 1354, 1356, 1357, 1359, 1360, 1361, 1357, 1360, 1362, 1366, 1373, 1380, 1399, 1403, 1414, 1435, 1439, 1424, 1433, 1439, 1446, 1443, 1444, 1438, 1435, 1434, 1426, 1441, 1451, 1452, 1463, 1472, 1468, 1461, 1489, 1485, 1487, 1492, 1489, 1492, 1489, 1497, 1468, 1469, 1473, 1477, 1476, 1477, 1502, 1499, 1501, 1506, 1502, 1505, 1512, 1543, 1541, 1556, 1557, 1555, 1557, 1556, 1530, 1548, 1545, 1552, 1559, 1569, 1570, 1555, 1563, 1562, 1565, 1574, 1573, 1574, 1573, 1563, 1572, 1576, 1571, 1573, 1565, 1570, 1571, 1572, 1579, 1590, 1591, 1595, 1597, 1600, 1596, 1597, 1596, 1605, 1607, 1606, 1607, 1610, 1612, 1614, 1620, 1625, 1639, 1636, 1639, 1634, 1633, 1624, 1644, 1645, 1642, 1651, 1654, 1658, 1663, 1665, 1666, 1668, 1671, 1676, 1663, 1656, 1659, 1656, 1655, 1650, 1656, 1657, 1667, 1673, 1674, 1675, 1676, 1680, 1673, 1674, 1677, 1683, 1690, 1692, 1697, 1693, 1694, 1701, 1714, 1689, 1691, 1692, 1684, 1686, 1685, 1683, 1685, 1686, 1692, 1706, 1694, 1692, 1702, 1703, 1723, 1728, 1726, 1728, 1729, 1730, 1738, 1737, 1703, 1701, 1714, 1729, 1730, 1728, 1729, 1730, 1729, 1707, 1714, 1755, 1760, 1786, 1787, 1807, 1814, 1815, 1813, 1808, 1810, 1794, 1797, 1801, 1800, 1807, 1802, 1803, 1804, 1819, 1821, 1819, 1828, 1816, 1817, 1852, 1839, 1840, 1837, 1838, 1877, 1876, 1868, 1879, 1886, 1885, 1886, 1859, 1864, 1872, 1905, 1907, 1912, 1915, 1920, 1936, 1935, 1943, 1945, 1968, 1963, 1954, 1969, 1968, 1976, 1977, 1978, 1963, 1961, 1966, 1968, 1983, 1984, 1983, 1982, 1954, 1957, 1959, 1970, 1980, 1986, 1996, 1997, 1964, 1957, 1958, 1957, 1969, 1982, 1988, 1985, 1986, 1982, 1981, 1980, 2007, 1999, 2002, 2033, 2039, 2040, 2028, 2048, 2049, 2047, 2046, 2051, 2049, 2036, 2028, 2041, 2038, 2041, 2044, 2052, 2058, 2060, 2063, 2068, 2071, 2070, 2076, 2075, 2055, 2054, 2049, 2035, 2038, 2048, 2071, 2090, 2097, 2102, 2086, 2085, 2083, 2085, 2090, 2107, 2106, 2138, 2144, 2151, 2142, 2153, 2154, 2155, 2156, 2159, 2155, 2178, 2181, 2182, 2189, 2190, 2194, 2197, 2198, 2194, 2198, 2201, 2202, 2201, 2199, 2171, 2169, 2164, 2178, 2181, 2186, 2187, 2188, 2191, 2190, 2193, 2211, 2226, 2241, 2227, 2253, 2259, 2281, 2282, 2283, 2282, 2264, 2263, 2278, 2295, 2299, 2290, 2291, 2294, 2307, 2306, 2315, 2314, 2316, 2315, 2340, 2339, 2350, 2362, 2365, 2322, 2339, 2340, 2353, 2368, 2363, 2361, 2370, 2371, 2376, 2368, 2369, 2371, 2372, 2394, 2408, 2415, 2417, 2399, 2405, 2408, 2410, 2417, 2418, 2425, 2426, 2424, 2423, 2453, 2456, 2467, 2475, 2478, 2480, 2487, 2503, 2506, 2502, 2508, 2494, 2497, 2502, 2505, 2511, 2512, 2509, 2516, 2511, 2516, 2523, 2493, 2500, 2502, 2506, 2507, 2483, 2486, 2484, 2454, 2456, 2467, 2470, 2473, 2485, 2484, 2491, 2492, 2496, 2497, 2518, 2483, 2484, 2511, 2516, 2485, 2486, 2487, 2482, 2483, 2484, 2481, 2508, 2512, 2503, 2511, 2542, 2536, 2538, 2547, 2540, 2531, 2532, 2535, 2526, 2525, 2552, 2562, 2563, 2564, 2563, 2562, 2561, 2564, 2565, 2567, 2568, 2563, 2564, 2576, 2577, 2582, 2573, 2569, 2564, 2570, 2568, 2571, 2582, 2564, 2566, 2564, 2579, 2588, 2587, 2590, 2592, 2598, 2611, 2634, 2653, 2652, 2648, 2649, 2650, 2636, 2641, 2660, 2661, 2659, 2661, 2653, 2654, 2640, 2638, 2636, 2635, 2633, 2635, 2636, 2656, 2657, 2658, 2667, 2679, 2707, 2697, 2708, 2702, 2703, 2714, 2715, 2722, 2726, 2730, 2720, 2724, 2725, 2733, 2745, 2748, 2750, 2746, 2736, 2737, 2739, 2740, 2720, 2729, 2734, 2726, 2739, 2734, 2736, 2735, 2736, 2735, 2737, 2727, 2694, 2692, 2702, 2703, 2710, 2713, 2708, 2710, 2721, 2731, 2722, 2723, 2724, 2742, 2750, 2745, 2735, 2745, 2748, 2778, 2770, 2772, 2773, 2772, 2773, 2768, 2779, 2778, 2779, 2802, 2801, 2809, 2810, 2814, 2833, 2835, 2844, 2859, 2860, 2861, 2865, 2870, 2872, 2870, 2876, 2875, 2877, 2883, 2872, 2871, 2872, 2891, 2889, 2890, 2868, 2866, 2864, 2867, 2862, 2863, 2864, 2867, 2870, 2874, 2875, 2895, 2887, 2886, 2887, 2886, 2889, 2890, 2891, 2890, 2867, 2888, 2895, 2896, 2891, 2894, 2896, 2886, 2888, 2892, 2878, 2871, 2876, 2877, 2875, 2874, 2875, 2887, 2875, 2901, 2920, 2923, 2924, 2925, 2907, 2908, 2931, 2932, 2933, 2937, 2951, 2952, 2951, 2947, 2967, 2966, 2962, 2972, 2953, 2941, 2962, 2988, 2995, 3014, 3008, 3009, 3024, 3034, 3029, 3038, 3058, 3059, 3060, 3037, 3040, 3039, 3030, 3046, 3058, 3059, 3067, 3069, 3082, 3086, 3088, 3089, 3086, 3097, 3105, 3116, 3114, 3148, 3152, 3154, 3164, 3196, 3198, 3200, 3212, 3217, 3224, 3247, 3249, 3250, 3251, 3256, 3243, 3247, 3249, 3250, 3249, 3254, 3255, 3256, 3273, 3274, 3275, 3273, 3274, 3276, 3263, 3271, 3272, 3290, 3289, 3290, 3292, 3279, 3284, 3273, 3277, 3284, 3289, 3292, 3301, 3300, 3338, 3340, 3375, 3384, 3382, 3375, 3377, 3373, 3376, 3375, 3382, 3385, 3404, 3400, 3403, 3415, 3408, 3420, 3421, 3422, 3419, 3423, 3432, 3434, 3435, 3436, 3438, 3431, 3426, 3434, 3460, 3462, 3463, 3438, 3445, 3446, 3451, 3448, 3468, 3460, 3453, 3459, 3464, 3465, 3459, 3465, 3462, 3464, 3474, 3489, 3501, 3502, 3503, 3506, 3507, 3515, 3505, 3508, 3509, 3508, 3510, 3494, 3495, 3494, 3495, 3496, 3497, 3500, 3513, 3512, 3513, 3514, 3531, 3533, 3531, 3523, 3524, 3528, 3550, 3549, 3538, 3532, 3529, 3535, 3526, 3529, 3528, 3563, 3533, 3534, 3541, 3543, 3544, 3543, 3565, 3576, 3581, 3622, 3624, 3630, 3622, 3628, 3640, 3649, 3644, 3645, 3655, 3654, 3648, 3649, 3644, 3642, 3643, 3668, 3672, 3688, 3718, 3724, 3734, 3737, 3758, 3757, 3760, 3761, 3777, 3775, 3770, 3767, 3768, 3767, 3774, 3771, 3778, 3776, 3782, 3790, 3791, 3803, 3819, 3824, 3822, 3828, 3819, 3805, 3809, 3808, 3809, 3815, 3817, 3819, 3817, 3821, 3842, 3846, 3844, 3842, 3857, 3858, 3876, 3869, 3878, 3848, 3850, 3840, 3862, 3861, 3859, 3869, 3872, 3865, 3864, 3859, 3861, 3854, 3853, 3854, 3850, 3851, 3860, 3866, 3850, 3846, 3847, 3864, 3852, 3859, 3857, 3860, 3859, 3860, 3862, 3877, 3880, 3881, 3914, 3911, 3913, 3923, 3924, 3934, 3936, 3927, 3934, 3939, 3940, 3942, 3936, 3937, 3936, 3956, 3936, 3937, 3942, 3943, 3951, 3954, 3966, 3978, 3980, 3992, 3997, 4001, 3991, 3990, 3994, 4000, 4007, 4010, 4014, 3990, 3991, 3992, 3995, 4005, 4006, 4009, 4022, 4023, 4026, 4003, 4017, 4018, 4020, 4021, 4023, 4026, 4028, 4027, 4016, 4015, 4011, 4001, 4027, 4031, 4033, 4045, 4059, 4065, 4063, 4064, 4065, 4070, 4075, 4076, 4069, 4052, 4054, 4024, 4017, 4018, 4017, 4018, 4015, 4011, 4014, 4015, 4022, 4009, 4008, 4009, 3988, 4010, 4025, 3990, 3998, 4002, 4008, 4006, 4008, 4015, 4013, 4010, 4019, 4035, 4034, 4036, 4044, 4073, 4070, 4067, 4068, 4086, 4087, 4088, 4085, 4084, 4083, 4086, 4083, 4082, 4083, 4080, 4077, 4049, 4050, 4052, 4053, 4057, 4065, 4068, 4065, 4083, 4084, 4101, 4110, 4114, 4113, 4111, 4112, 4116, 4094, 4101, 4105, 4104, 4107, 4106, 4120, 4138, 4147, 4148, 4152, 4157, 4160, 4161, 4163, 4155, 4153, 4156, 4178, 4179, 4184, 4203, 4206, 4208, 4212, 4196, 4197, 4169, 4173, 4159, 4160, 4161, 4162, 4163, 4160, 4162, 4164, 4165, 4166, 4168, 4166, 4167, 4157, 4166, 4157, 4159, 4175, 4173, 4162, 4153, 4137, 4142, 4144, 4145, 4146, 4152, 4158, 4173, 4171, 4167, 4168, 4173, 4172, 4173, 4178, 4181, 4161, 4164, 4160, 4153, 4155, 4154, 4149, 4160, 4162, 4178, 4172, 4174, 4183, 4184, 4192, 4198, 4201, 4204, 4210, 4211, 4216, 4222, 4232, 4233, 4248, 4250, 4254, 4255, 4270, 4275, 4276, 4282, 4289, 4287, 4288, 4290, 4291, 4319, 4334, 4352, 4360, 4367, 4377, 4384, 4407, 4408, 4407, 4419, 4404, 4407, 4423, 4425, 4424, 4428, 4429, 4451, 4446, 4444, 4445, 4449, 4472, 4474, 4475, 4486, 4496, 4470, 4473, 4478, 4477, 4478, 4480, 4491, 4492, 4487, 4486, 4494, 4499, 4521, 4520, 4532, 4536, 4510, 4516, 4526, 4534, 4515, 4509, 4510, 4511, 4472, 4473, 4483, 4477, 4478, 4497, 4522, 4524, 4550, 4553, 4558, 4559, 4543, 4534, 4533, 4521, 4525, 4526, 4514, 4520, 4515, 4503, 4506, 4511, 4529, 4530, 4536, 4538, 4551, 4553, 4558, 4561, 4545, 4542, 4546, 4534, 4533, 4535, 4539, 4536, 4532, 4541, 4545, 4538, 4535, 4516, 4514, 4513, 4538, 4549, 4556, 4562, 4563, 4564, 4568, 4570, 4575, 4576, 4591, 4577, 4589, 4590, 4571, 4576, 4574, 4575, 4576, 4574, 4577, 4574, 4575, 4588, 4580, 4598, 4595, 4610, 4603, 4604, 4605, 4593, 4594, 4580, 4583, 4604, 4613, 4614, 4631, 4629, 4647, 4628, 4615, 4634, 4635, 4636, 4640, 4646, 4652, 4651, 4653, 4649, 4643, 4645, 4642, 4643, 4639, 4644, 4655, 4658, 4659, 4660, 4668, 4669, 4670, 4680, 4692, 4682, 4686, 4688, 4691, 4690, 4697, 4698, 4700, 4703, 4709, 4713, 4718, 4720, 4728, 4742, 4744, 4743, 4751, 4753, 4741, 4747, 4750, 4751, 4733, 4732, 4734, 4723, 4724, 4738, 4739, 4767, 4773, 4775, 4778, 4782, 4786, 4790, 4754, 4757, 4769, 4763, 4768, 4770, 4773, 4798, 4818, 4819, 4820, 4819, 4820, 4822, 4830, 4828, 4830, 4815, 4821, 4828, 4826, 4829, 4828, 4824, 4823, 4827, 4809, 4816, 4818, 4819, 4821, 4822, 4833, 4837, 4834, 4835, 4838, 4848, 4855, 4866, 4864, 4859, 4879, 4882, 4880, 4878, 4860, 4847, 4850, 4835, 4836, 4840, 4827, 4849, 4848, 4849, 4855, 4847, 4842, 4846, 4849, 4807, 4809, 4815, 4842, 4847, 4863, 4846, 4845, 4844, 4846, 4849, 4861, 4841, 4861, 4862, 4881, 4880, 4890, 4892, 4895, 4916, 4917, 4923, 4943, 4945, 4944, 4946, 4957, 4955, 4943, 4939, 4955, 4957, 4961, 4962, 4970, 4972, 4973, 4979, 4964, 4963, 4960, 4961, 4955, 4946, 4951, 4949, 4962, 4964, 4968, 4976, 4965, 4967, 4961, 4965, 4966, 4953, 4923, 4928, 4929, 4932, 4934, 4936, 4951, 4949, 4950, 4976, 4975, 4996, 4999, 5007, 5011, 5010, 5004, 5017, 5022, 5016, 5015, 5022, 5016, 5025, 5040, 5033, 5037, 5024, 5025, 5028, 5030, 5026, 5025, 5028, 5029, 5035, 5034, 5058, 5061, 5060, 5065, 5049, 5056, 5055, 5050, 5068, 5069, 5074, 5090, 5083, 5068, 5080, 5079, 5080, 5081, 5084, 5099, 5100, 5115, 5114, 5119, 5120, 5128, 5120, 5131, 5130, 5120, 5113, 5126, 5130, 5156, 5158, 5159, 5163, 5162, 5125, 5126, 5135, 5162, 5166, 5173, 5169, 5177, 5183, 5199, 5200, 5188, 5191, 5192, 5184, 5193, 5203, 5206, 5214, 5219, 5215, 5230, 5231, 5208, 5207, 5208, 5181, 5161, 5162, 5164, 5189, 5190, 5170)
def solve1(xs: List[Int]): Int = xs match
  case x::y::rest => (if x < y then 1 else 0) + solve1(y::rest)
  case _ => 0

def sliding_window(xs: List[Int]): List[(Int,Int,Int)] = xs match
  case x::y::z::rest => (x,y,z) :: sliding_window(y::z::rest)
  case _ => Nil

def solve2(xs: List[Int]): Int =
  solve1 (for (a,b,c) <- sliding_window(xs) yield a+b+c)


def main(): Unit =
  println(f"Part 1: ${solve1(inputs)}")
  println(f"Part 2: ${solve2(inputs)}")

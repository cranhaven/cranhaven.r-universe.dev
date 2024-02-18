test_that("Quaternion", {

  library(testthat)
  library(ravetools)
  q <- new_quaternion()
  q1 <- new_quaternion()

  expect_equal(q$to_array(), c(0, 0, 0, 1))


  # JS Code:
  # new THREE.Quaternion().setFromAxisAngle(new THREE.Vector3().set(1,2,3), Math.PI / 6).toArray()
  expect_equal(
    q$set_from_axis_angle(new_vector3(1,2,3)$normalize(), pi/6)[],
    c(0.06917229942468747, 0.13834459884937494, 0.20751689827406242, 0.9659258262890683)
  )

  # JS Code:
  # new THREE.Quaternion().setFromRotationMatrix(new THREE.Matrix4().set(1,2,3,4,5,6,7,8,9,10,11,12,0,0,0,1)).toArray()
  expect_equal(
    q$set_from_rotation_matrix(new_matrix4()$set(1:12, 0,0,0,1))[],
    c(0.3441236008058426, -0.6882472016116852, 0.3441236008058426, 2.179449471770337)
  )

  # JS Code:
  # new THREE.Quaternion().setFromUnitVectors(new THREE.Vector3().set(1,2,3).normalize(), new THREE.Vector3().set(3,2,1).normalize()).toArray()
  expect_equal(
    q$set_from_unit_vectors(new_vector3(1,2,3)$normalize(), new_vector3(3,2,1)$normalize())[],
    c(-0.15430334996209188, 0.3086066999241838, -0.15430334996209188, 0.9258200997725514)
  )

  # JS Code:
  # new THREE.Quaternion().set(1,2,3,4).normalize().angle_to(new THREE.Quaternion().set(4,3,2,1).normalize())
  expect_equal(
    q$set(1,2,3,4)$normalize()$angle_to(q1$set(4,3,2,1)$normalize()),
    1.6821373411358609
  )

  # JS Code:
  # new THREE.Quaternion().set(1,2,3,4).normalize().rotateTowards(new THREE.Quaternion().set(4,3,2,1).normalize(), 0.5).toArray()
  expect_equal(
    q$set(1,2,3,4)$normalize()$rotate_towards(q1$set(4,3,2,1)$normalize(), 0.5)[],
    c(0.37890288337859324, 0.45479903649823583, 0.5306951896178784, 0.6065913427375211)
  )

  # JS Code:
  # new THREE.Quaternion().set(1,2,3,4).normalize().slerp(new THREE.Quaternion().set(4,3,2,1).normalize(), 0.5).toArray()
  expect_equal(
    q$set(1,2,3,4)$normalize()$slerp(q1$set(4,3,2,1)$normalize(), 0.1)[],
    c(0.2505208376973254, 0.3981536259164245, 0.5457864141355235, 0.6934192023546226)
  )

  expect_equal(
    q$identity()[],
    c(0,0,0,1)
  )
  expect_equal(
    q$set(0.5,0.5,0.5,0.5)$invert()[],
    c(-0.5,-0.5,-0.5,0.5)
  )

  # new THREE.Quaternion().set(1,2,3,4).normalize().dot(new THREE.Quaternion().set(4,3,2,1).normalize())
  expect_equal(
    q$set(1,2,3,4)$normalize()$dot(q1$set(4,3,2,1)$normalize()),
    sum((1:4) * (4:1)) / sum((1:4)^2)
  )

  expect_equal(
    q$set(1,2,3,4)$length_squared(),
    sum((1:4)^2)
  )

  expect_equal(
    q$set(1,2,3,4)$length(),
    sqrt(sum((1:4)^2))
  )

  # q = new THREE.Quaternion(); q.multipleQuaternions(new THREE.Quaternion().set(4,3,2,1), q.set(1,2,3,4)).toArray()
  expect_equal(
    q$multiply_quaternions(q1$set(4,3,2,1), q$set(1,2,3,4))[],
    c(22, 4, 16, -12)
  )

  # new THREE.Quaternion().set(1,2,3,4).multiply(new THREE.Quaternion().set(4,3,2,1)).toArray()
  expect_equal(
    q$set(1,2,3,4)$multiply(q1$set(4,3,2,1))[],
    c(12, 24, 6, -12)
  )

  # new THREE.Quaternion().set(1,2,3,4).premultiply(new THREE.Quaternion().set(4,3,2,1)).toArray()
  q$set(1,2,3,4)
  q1$set(4,3,2,1)

  expect_equal(q[], 1:4)
  expect_equal(q1[], 4:1)

  expect_equal(c(q$x, q$y, q$z, q$w), 1:4)
  expect_equal(c(q1$x, q1$y, q1$z, q1$w), 4:1)

  q$premultiply(q1)
  if(!all(q[] == c(22, 4, 16, -12))) {
    stop(format(q), "\n", q$x, " ", q$y, " ", q$z, " ", q$w)
  }

  expect_equal(
    q[],
    c(22, 4, 16, -12)
  )
  # expect_equal(
  #   q$set(1,2,3,4)$premultiply(q1$set(4,3,2,1))[],
  #   c(22, 4, 16, -12)
  # )


})

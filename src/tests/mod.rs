use crate::block::Direction::Forward;
use crate::block::{BlockLattice, BlockUpdate, DataFlowGraph};
use crate::ir::JumpOperation;
use crate::ir::ops::BinaryOp::{Add, Sub};
use crate::ir::ops::UnaryOp::{Unit};
use crate::ir::StorageType::{Const, Variable};
use crate::ir::IR::{Jump, Assignment};
use crate::ir::{
    AddressMarker, CodeBlock, CodeBlockGraphWeight, IRInformation, StorageType,
};
use crate::reach_lattice::ReachLattice;
use crate::semilattice::{SemiLattice, SemiLatticeOrd};

mod u32_lattice;
#[test]
fn it_works() {
    let x = u32_lattice::U32SemiLattice::from(0b101101u32);
    let y = u32_lattice::U32SemiLattice::from(0b110011u32);
    let res = x.meet(&y);
    assert_eq!(res, 0b111111u32);
    assert_eq!(x.meet(&0), x);
    assert_eq!(x.meet(&u32::MAX), u32::MAX);
    assert!(x.lte(&0))
}
#[test]
fn graph() {
    let mut graph = DataFlowGraph::<u32_lattice::U32Block>::new(());
    let b1 = graph.graph.add_node(u32_lattice::U32Block::new(
        1.into(),
        0b10010,
        0b01101,
    ));
    let b2 = graph.graph.add_node(u32_lattice::U32Block::new(
        2.into(),
        0b01110,
        0b01110,
    ));
    graph.graph.add_edge(graph.entry, b1, ());
    graph.graph.add_edge(b1, b2, ());
    graph.converge(crate::block::Direction::Forward);
    graph.graph.node_indices().for_each(|i| {
        let node = graph.graph.node_weight(i).unwrap();
        println!("{}, {}", node.get_in(), node.get_out());
    });
}
#[test]
fn block_partition() {
    let i = 0;
    let j = 1;
    let k = 2;
    let t1 = 3;
    let irs = vec![
        // Assignment(Assign, i, Const(1), StorageType::None, IRInformation::default()),
        // Assignment(Assign, j, Const(1), StorageType::None, IRInformation::default()),
        // Assignment(Assign, k, Const(2), StorageType::None, IRInformation::default()),
        // Jump(
        //     E,
        //     AddressMarker::new(5),
        //     Variable(j),
        //     Variable(k),
        //     IRInformation::default(),
        // ),
        // Assignment(
        //     QuadType::E,
        //     t1,
        //     Variable(j),
        //     Variable(k),
        //     IRInformation::default(),
        // ),
        // Assignment(
        //     Assign,
        //     k,
        //     Variable(t1),
        //     StorageType::None,
        //     IRInformation::default(),
        // ),
        // Assignment(
        //     Assign,
        //     k,
        //     Variable(t1),
        //     StorageType::None,
        //     IRInformation::default(),
        // ),
        // Jump(
        //     Bool,
        //     AddressMarker::new(1),
        //     Variable(t1),
        //     StorageType::None,
        //     IRInformation::default(),
        // ),
        // Assignment(
        //     Assign,
        //     k,
        //     Variable(t1),
        //     StorageType::None,
        //     IRInformation::default(),
        // ),
    ];
    let mut partitioned = DataFlowGraph::from(irs);
    <DataFlowGraph<CodeBlock, CodeBlockGraphWeight> as BlockUpdate<ReachLattice>>::converge(
        &mut partitioned,
        Forward,
    );
    println!("{:}", partitioned)
}

#[test]
fn block_reach() {
    let i = 0;
    let j = 1;
    let a = 2;
    let m = 3;
    let n = 4;
    let u1 = 5;
    let u2 = 6;
    let u3 = 7;
    let irs = vec![
        // Assignment(Sub, i, Variable(m), Const(1), IRInformation::default()),
        // Assignment(
        //     Assign,
        //     j,
        //     Variable(n),
        //     StorageType::None,
        //     IRInformation::default(),
        // ),
        // Assignment(
        //     Assign,
        //     a,
        //     Variable(u1),
        //     StorageType::None,
        //     IRInformation::default(),
        // ),
        // Assignment(Add, i, Variable(i), Const(1), IRInformation::default()),
        // Assignment(Sub, j, Variable(j), Const(1), IRInformation::default()),
        // Jump(
        //     Bool,
        //     AddressMarker::new(7),
        //     StorageType::None,
        //     StorageType::None,
        //     IRInformation::default(),
        // ),
        // Assignment(
        //     Assign,
        //     a,
        //     Variable(u2),
        //     StorageType::None,
        //     IRInformation::default(),
        // ),
        // Assignment(
        //     Assign,
        //     i,
        //     Variable(u3),
        //     StorageType::None,
        //     IRInformation::default(),
        // ),
        // Jump(
        //     Bool,
        //     AddressMarker::new(3),
        //     StorageType::None,
        //     StorageType::None,
        //     IRInformation::default(),
        // ),
    ];
    let mut partitioned = DataFlowGraph::from(irs);
    <DataFlowGraph<CodeBlock, CodeBlockGraphWeight> as BlockUpdate<ReachLattice>>::converge(
        &mut partitioned,
        Forward,
    );
    println!("{:}", partitioned)
}
